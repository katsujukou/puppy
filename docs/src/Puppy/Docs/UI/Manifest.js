// The manifest and the pages it names, inlined at bundle time. `import.meta.glob`
// is eager so that every rendered page is in the bundle: there is no server to
// fetch them from. Paths resolve from output/<Module>/, where PureScript puts the
// compiled module this file belongs to.
import M from "../../docs/out/manifest.json" with { type: "json" };

const htmls = import.meta.glob("../../docs/out/**/*.html", {
  query: "?raw",
  import: "default",
  eager: true,
});

const htmlOf = (rel) => htmls["../../docs/out/" + rel] ?? "";

const withHtml = (p) => ({ ...p, html: htmlOf(p.html) });

export const manifestJson = {
  landing: withHtml(M.landing),
  pages: M.pages.map(withHtml),
};
