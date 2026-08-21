// Scroll to the element matching the current URL hash. The Markdown HTML is
// injected via innerHTML after the route resolves, so the browser's own
// on-load hash scroll runs too early (the heading doesn't exist yet). We
// re-trigger it from a lifecycle effect, on the next frame to be safe.
export const scrollToCurrentHash = () => {
  const id = decodeURIComponent(window.location.hash.replace(/^#/, ""));
  if (!id) return;
  requestAnimationFrame(() => {
    const el = document.getElementById(id);
    if (el) el.scrollIntoView({ block: "start" });
  });
};

// Root-absolute links authored in the Markdown (e.g. /dev/optimizations.md) are
// missing the deploy base path, so prefix it here after the HTML is injected.
// Idempotent, and a no-op when served from the root.
export const prefixInternalLinks = () => {
  const base = (import.meta.env && import.meta.env.BASE_URL) || "/";
  if (base === "/") return;
  const prefix = base.replace(/\/$/, "");
  document.querySelectorAll(".md-doc a[href^='/']").forEach((a) => {
    const href = a.getAttribute("href");
    if (href.startsWith("//") || href.startsWith(prefix + "/")) return;
    a.setAttribute("href", prefix + href);
  });
};

// Click a benchmark chart to view it enlarged in a lightbox overlay. Scoped to
// images served from images/bench/. Uses event delegation on the .md-doc root so
// it covers the innerHTML-injected images and is cleaned up with it on unmount.
const BENCH_IMG = 'img[src*="/images/bench/"]';

export const enableImageZoom = () => {
  const root = document.querySelector(".md-doc");
  if (!root) return;
  root.querySelectorAll(BENCH_IMG).forEach((img) => img.classList.add("zoomable"));
  root.addEventListener("click", (e) => {
    const img = e.target;
    if (!img || img.tagName !== "IMG" || !img.matches(BENCH_IMG)) return;
    openLightbox(img.currentSrc || img.src, img.alt || "");
  });
};

const closeLightbox = () => {
  const ex = document.querySelector(".img-lightbox");
  if (!ex) return;
  if (ex._onKey) document.removeEventListener("keydown", ex._onKey);
  ex.remove();
  document.body.style.overflow = "";
};

const openLightbox = (src, alt) => {
  closeLightbox();
  const overlay = document.createElement("div");
  overlay.className = "img-lightbox";

  const img = document.createElement("img");
  img.src = src;
  img.alt = alt;
  overlay.appendChild(img);

  const onKey = (e) => {
    if (e.key === "Escape") closeLightbox();
  };
  overlay._onKey = onKey;
  overlay.addEventListener("click", closeLightbox);
  document.addEventListener("keydown", onKey);

  document.body.appendChild(overlay);
  document.body.style.overflow = "hidden";
};
