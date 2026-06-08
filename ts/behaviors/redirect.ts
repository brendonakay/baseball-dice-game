// "redirect" behavior — declarative client-side navigation.
//
// Replaces the former inline `<script>setTimeout(... window.location ...)`
// hacks in the game views. The server now renders a plain element such as:
//
//   <div data-behavior="redirect" data-redirect-url="/user" data-redirect-delay="1000"></div>
//
// and this behavior reads the attributes and performs the navigation. No
// JavaScript lives in the HTML DSL anymore — only declarative data.

import { numAttr } from "../lib/dom";

export const redirect = (el: HTMLElement): void => {
  const url = el.getAttribute("data-redirect-url");
  if (!url) return;
  const delay = numAttr(el, "data-redirect-delay", 0);
  window.setTimeout(() => {
    window.location.href = url;
  }, delay);
};
