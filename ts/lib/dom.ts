// Small, typed DOM helpers so behaviors stay declarative and the
// side-effecting surface area lives in one place.

/** Query all elements matching `selector` within `root` as a typed array. */
export const qsa = <E extends Element = HTMLElement>(
  selector: string,
  root: ParentNode = document,
): E[] => Array.from(root.querySelectorAll<E>(selector));

/** Read a numeric data attribute, falling back to `fallback` when absent/NaN. */
export const numAttr = (el: Element, name: string, fallback: number): number => {
  const raw = el.getAttribute(name);
  if (raw === null) return fallback;
  const n = Number(raw);
  return Number.isFinite(n) ? n : fallback;
};

/** Run `fn` once the DOM is ready (or immediately if it already is). */
export const onReady = (fn: () => void): void => {
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", fn, { once: true });
  } else {
    fn();
  }
};
