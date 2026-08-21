// The path Vite was told to serve the site from: "/" from the root, "/puppy/"
// for a GitHub Pages project page. Vite substitutes this at build time; the
// fallback is for when the module is loaded outside a Vite build.
export const baseUrl =
  (typeof import.meta.env === "object" && import.meta.env.BASE_URL) || "/";
