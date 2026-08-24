import { execFileSync } from "node:child_process";
import { randomBytes } from "node:crypto";
import {
  closeSync,
  readSync,
  lstatSync,
  mkdirSync,
  openSync,
  readdirSync,
  readFileSync,
  renameSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { basename, dirname, join, relative, resolve } from "node:path";

const problem = (path, e) => ({ path, reason: e.message });

export const readTextImpl = (failed) => (ok) => (path) => () => {
  try {
    return ok(readFileSync(path, "utf8"));
  } catch (e) {
    return failed(problem(path, e));
  }
};

// The same, for a file that need not be there.
//
// `ENOENT` is the ordinary answer: nothing has been written here yet.
// Everything else is a failure -- a file that exists and cannot be read is not
// a file to overwrite on the assumption that it does not matter.
export const readIfPresentImpl = (failed) => (missing) => (ok) => (path) => () => {
  try {
    return ok(readFileSync(path, "utf8"));
  } catch (e) {
    return e.code === "ENOENT" ? missing : failed(problem(path, e));
  }
};

// Written to a neighbour and moved into place, so that a failure part way
// through leaves whatever was there before rather than half a module.
//
// The neighbour is named afresh each time and created with `wx`, which fails
// rather than opening something that is already there. A fixed name would let
// two runs writing the same module take turns clobbering each other's scratch
// file, and the one that finished first could find its output changed
// afterwards.
export const writeTextImpl = (failed) => (ok) => (path) => (contents) => () => {
  const dir = dirname(path);
  const base = basename(path);
  for (let attempt = 0; attempt < 8; attempt++) {
    const scratch = join(
      dir,
      "." + base + "." + randomBytes(8).toString("hex") + ".puppy-tmp",
    );
    let handle;
    try {
      handle = openSync(scratch, "wx");
    } catch (e) {
      if (e.code === "EEXIST") continue;
      return failed(problem(path, e));
    }
    try {
      writeFileSync(handle, contents, "utf8");
      closeSync(handle);
      renameSync(scratch, path);
      return ok;
    } catch (e) {
      try {
        closeSync(handle);
      } catch {
        // Already closed, or never opened cleanly.
      }
      try {
        rmSync(scratch, { force: true });
      } catch {
        // The scratch file is the lesser problem.
      }
      return failed(problem(path, e));
    }
  }
  return failed({
    path,
    reason: "could not make a temporary file to write through",
  });
};

// `ENOENT` is the one error with an ordinary meaning here: no such directory,
// so nothing in it. Everything else is a failure with a path attached.
export const readDirImpl = (failed) => (missing) => (ok) => (path) => () => {
  try {
    return ok(readdirSync(path));
  } catch (e) {
    return e.code === "ENOENT" ? missing : failed(problem(path, e));
  }
};

// `lstat`, not `stat`: a symlink to a directory is not followed, which keeps a
// walk inside the package it started in and out of any cycle a link could make.
export const isDirectoryImpl = (failed) => (ok) => (path) => () => {
  try {
    return ok(lstatSync(path).isDirectory());
  } catch (e) {
    return e.code === "ENOENT" ? ok(false) : failed(problem(path, e));
  }
};

export const mkdirPImpl = (failed) => (ok) => (path) => () => {
  try {
    mkdirSync(path, { recursive: true });
    return ok;
  } catch (e) {
    return failed(problem(path, e));
  }
};

export const removeImpl = (failed) => (ok) => (path) => () => {
  try {
    rmSync(path, { force: true });
    return ok;
  } catch (e) {
    return failed(problem(path, e));
  }
};

// Two names reach the same file when they resolve alike, and also when a link
// puts them on one inode.
//
// Each side is looked at on its own. Something that is not there cannot be the
// file being read from, so it is not the same; anything else that stops the
// question being answered is a failure naming the path that stopped it, rather
// than a quiet "no".
export const sameFileImpl = (failed) => (ok) => (a) => (b) => () => {
  if (resolve(a) === resolve(b)) return ok(true);

  const look = (path) => {
    try {
      return { stats: statSync(path) };
    } catch (e) {
      return e.code === "ENOENT"
        ? { missing: true }
        : { error: problem(path, e) };
    }
  };

  const left = look(a);
  if (left.error) return failed(left.error);
  if (left.missing) return ok(false);

  const right = look(b);
  if (right.error) return failed(right.error);
  if (right.missing) return ok(false);

  return ok(
    left.stats.dev === right.stats.dev && left.stats.ino === right.stats.ino,
  );
};

export const captureImpl = (command) => (args) => () =>
  execFileSync(command, args, {
    encoding: "utf8",
    stdio: ["ignore", "pipe", "inherit"],
  });

// How someone standing here would write this path.
//
// Under the current directory that is the short form. Outside it the relative
// form is a chain of `..` that is longer than the absolute path and harder to
// read, so the absolute path wins.
export const relativeImpl = (path) => () => {
  const here = relative(process.cwd(), path);
  if (here === "") return ".";
  return here.startsWith("..") ? path : here;
};

// A yes-or-no question, put to whoever is running the tool.
//
// Only asked when standard input is a terminal. In a build script, a CI job or
// a git hook there is nobody there, and a tool that waits for an answer waits
// until it is killed -- so it says there was nobody to ask and lets the caller
// decide what that means.
//
// The question goes to standard error: standard output is the list of modules
// written, and something may be reading it.
//
// Read a byte at a time rather than through `readline`, which has no
// synchronous form.
//
// `EAGAIN` means the terminal is in non-blocking mode, which some shells leave
// behind; there is nothing for it but to ask again. Asking again immediately
// would spin a core flat for as long as the person takes to answer, so the wait
// is a real one: `Atomics.wait` blocks this thread without running anything,
// which is the only way to sleep without an event loop to come back to.
const idle = new Int32Array(new SharedArrayBuffer(4));

export const confirmImpl = (no) => (yes) => (noOneToAsk) => (question) => () => {
  if (!process.stdin.isTTY) return noOneToAsk;

  process.stderr.write(question);

  const byte = Buffer.alloc(1);
  let answer = "";
  for (;;) {
    let read;
    try {
      read = readSync(0, byte, 0, 1, null);
    } catch (e) {
      if (e.code === "EAGAIN") {
        Atomics.wait(idle, 0, 0, 20);
        continue;
      }
      if (e.code === "EOF") break;
      return noOneToAsk;
    }
    if (read === 0) break;
    const character = byte.toString("utf8");
    if (character === "\n") break;
    answer += character;
  }

  const said = answer.trim().toLowerCase();
  return said === "y" || said === "yes" ? yes : no;
};
