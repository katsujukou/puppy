// The page scrolls inside the content column, not the window: the layout is a
// full-height flex box, and only that column has `overflow-y-auto`. So
// `window.scrollTo` has nothing to do, and this has to reach the element.
//
// A URL that names an anchor is left alone. MarkdownView scrolls to the
// heading once its HTML is in the DOM, and resetting here would undo it.
export const resetImpl = (id) => () => {
  if (window.location.hash) return;
  const el = document.getElementById(id);
  if (el) el.scrollTop = 0;
};
