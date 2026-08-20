import { execFileSync } from "node:child_process";
import { randomBytes } from "node:crypto";
import {
  closeSync,
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
import { basename, dirname, join, resolve } from "node:path";

const problem = (path, e) => ({ path, reason: e.message });

export const readTextImpl = (failed) => (ok) => (path) => () => {
  try {
    return ok(readFileSync(path, "utf8"));
  } catch (e) {
    return failed(problem(path, e));
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
