import { promises as fs } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { marked } from "marked";
import { gfmHeadingId } from "marked-gfm-heading-id";
import { createHighlighter } from "shiki";

const REPO_ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../..");
const CONTENT_DIR = path.join(REPO_ROOT, "docs");
const CONTENT_BASE = path.join(REPO_ROOT, "docs", "md");

const PAGES = [
  { file: "getting-started.md", nav: "Getting Started" },
  { file: "grammar.md", nav: "The Grammar File" },
  { file: "generated.md", nav: "The Generated Module" },
  { file: "cli.md", nav: "The Command Line" },
  { file: "conflicts.md", nav: "Conflicts" },
];

// The landing page. It is not in PAGES: those are the contents list, and this
// is what the site opens on.
const LANDING = { file: "README.md" };

const THEMES = { light: "github-light", dark: "github-dark" };

const LANG_ALIASES = {
  purescript: "purescript",
  purs: "purescript",
};

const GH_BLOB = "https://github.com/katsujukou/puppy/blob/main/docs";

// Markdown path relative to the docs root -> the route the app serves it at.
// Links are written between .md files; the reader should get the route.
const routeMap = {};
for (const p of PAGES) {
  routeMap[`md/${p.file}`] = `/${p.file.replace(/\.(md|markdown)$/i, "")}`;
}
routeMap[`md/${LANDING.file}`] = "/";

// Rewrite the cross-document .md links in rendered HTML (the upstream docs link
// each other with relative paths like "./grammar.md#anchor"). `fileDir` is the
// linking file's directory relative to the docs root.
function rewriteLinks(html, fileDir) {
  return html.replace(/href="([^"]+)"/g, (whole, href) => {
    if (/^(https?:|mailto:|#|\/)/i.test(href)) return whole; // external / anchor / already absolute
    const [rawPath, anchor] = href.split("#");
    if (!/\.(md|markdown)$/i.test(rawPath)) return whole;
    const rel = path.posix.normalize(path.posix.join(fileDir, rawPath));
    const frag = anchor ? `#${anchor}` : "";
    if (routeMap[rel]) return `href="${routeMap[rel]}${frag}"`;
    return whole;
  });
}

// Turn `> **Note** …` callouts into an icon: tag the blockquote `note` and swap
// the leading bold "Note" label for an info icon span (styled/themed in index.css).
function decorateNotes(html) {
  return html.replace(
    /<blockquote>\s*<p><strong>Note<\/strong>/g,
    '<blockquote class="note">\n<p><span class="note-icon" role="img" aria-label="Note"></span>',
  );
}

const highlighter = await createHighlighter({
  themes: [THEMES.light, THEMES.dark],
  langs: ["purescript"],
});

const escapeHtml = (s) =>
  s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");

const stripTags = (s) =>
  s
    .replace(/<[^>]*>/g, " ")
    .replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .replace(/&quot;/g, '"')
    .replace(/&#39;/g, "'")
    .replace(/&amp;/g, "&")
    .replace(/\s+/g, " ")
    .trim();

// First level-1 heading, with inline markdown stripped — used as the page title
// (search docTitle and the default sidebar label). Falls back to the filename.
function titleOf(md, file) {
  const m = md.match(/^#\s+(.+?)\s*$/m);
  if (m) return m[1].replace(/`/g, "").replace(/[*_]/g, "").trim();
  return path.basename(file).replace(/\.(md|markdown)$/i, "");
}

// Split rendered HTML into one search record per heading section, carrying the
// heading text, its anchor id, and the plain text of the body that follows.
function sectionsOf(html, route) {
  const records = [];
  const re = /<h([1-6]) id="([^"]*)"[^>]*>([\s\S]*?)<\/h\1>/g;
  const headings = [];
  let m;
  while ((m = re.exec(html))) {
    headings.push({ index: m.index, end: re.lastIndex, anchor: m[2], heading: stripTags(m[3]) });
  }
  for (let i = 0; i < headings.length; i++) {
    const h = headings[i];
    const bodyEnd = i + 1 < headings.length ? headings[i + 1].index : html.length;
    records.push({
      id: `${route.path}#${h.anchor}`,
      path: route.path,
      anchor: h.anchor,
      docTitle: route.title,
      heading: h.heading,
      text: stripTags(html.slice(h.end, bodyEnd)),
    });
  }
  return records;
}

// GitHub-style heading ids, e.g. "## Worked example: the Effect monad" ->
// id="worked-example-the-effect-monad", matching the TOC anchors in the source.
marked.use(gfmHeadingId());

// Override the code-block renderer to emit shiki-highlighted HTML.
marked.use({
  renderer: {
    code({ text, lang }) {
      const key = (lang ?? "").trim().split(/\s+/)[0].toLowerCase();
      const shikiLang = LANG_ALIASES[key];
      if (shikiLang) {
        return highlighter.codeToHtml(text, { lang: shikiLang, themes: THEMES });
      }
      return `<pre><code>${escapeHtml(text)}</code></pre>`;
    },
  },
});

const exists = (p) => fs.stat(p).then(() => true, () => false);

// Render one Markdown file to <dst>/<relHtml>, returning { title, html }. When
// `linkDir` is given, cross-document .md links are rewritten to app routes.
async function renderFile(srcFile, relHtml, linkDir) {
  const md = await fs.readFile(srcFile, "utf8");
  let html = decorateNotes(marked.parse(md));
  if (linkDir != null) html = rewriteLinks(html, linkDir);
  const out = path.join("out", relHtml);
  await fs.mkdir(path.dirname(out), { recursive: true });
  await fs.writeFile(out, html);
  return { title: titleOf(md, srcFile), html };
}

const searchIndex = [];

// Resolve a section page descriptor to its source file + rendered output path.
function pageSource(file) {
  const rel = `${file.replace(/\.(md|markdown)$/i, ".html")}`;
  return { src: path.join('md', file), rel };
}

// --- Render each section, building the manifest -----------------------------
const manifest = { pages: [] };
let pageCount = 0;

for (const page of PAGES) {
  const { src, rel } = pageSource(page.file);
  if (!(await exists(src))) {
    console.error(`[render-docs] WARNING: ${src} not found, skipping`);
    continue;
  }
  const routePath = `/${page.file.replace(/\.(md|markdown)$/i, "")}`;
  const { title, html } = await renderFile(src, rel, "md");
  manifest.pages.push({ path: routePath, title, nav: page.nav ?? title, html: rel });
  searchIndex.push(...sectionsOf(html, { path: routePath, title }));
  pageCount++;
}

// The landing page goes in on its own, so that `pages` stays the contents list.
{
  const { src, rel } = pageSource(LANDING.file);
  const { title, html } = await renderFile(src, rel, "md");
  manifest.landing = { path: "/", title, nav: title, html: rel };
  searchIndex.push(...sectionsOf(html, { path: "/", title }));
  pageCount++;
}

await fs.writeFile(path.join("out", "manifest.json"), JSON.stringify(manifest));
await fs.writeFile(path.join("out", "search-index.json"), JSON.stringify(searchIndex));

console.error(
  `[render-docs] ${pageCount} pages, ${searchIndex.length} search sections`,
);
