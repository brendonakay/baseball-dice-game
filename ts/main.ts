// WaxBall front-end entry point.
//
// Philosophy: HTMX drives the page. This bundle only adds small, functional
// "islands" of client behavior. A behavior is a pure-ish `(el) => void` that is
// attached to any element carrying `data-behavior="<name>"`. We re-scan whenever
// HTMX swaps new content in, so islands work on dynamically loaded fragments.

import { qsa, onReady } from "./lib/dom";
import { redirect } from "./behaviors/redirect";
import { registerToastListener } from "./behaviors/toast";

type Behavior = (el: HTMLElement) => void;

const behaviors: Record<string, Behavior> = {
  redirect,
};

const PROCESSED = "data-behavior-init";

/** Attach behaviors to every not-yet-initialized island under `root`. */
const scan = (root: ParentNode): void => {
  for (const el of qsa<HTMLElement>("[data-behavior]", root)) {
    if (el.hasAttribute(PROCESSED)) continue;
    const behavior = behaviors[el.getAttribute("data-behavior") ?? ""];
    if (!behavior) continue;
    el.setAttribute(PROCESSED, "");
    behavior(el);
  }
};

onReady(() => {
  registerToastListener();
  scan(document.body);

  // HTMX fires `htmx:load` on each newly swapped-in fragment.
  document.body.addEventListener("htmx:load", (event: Event) => {
    const target = (event as CustomEvent<{ elt?: HTMLElement }>).detail?.elt;
    scan(target ?? document.body);
  });
});
