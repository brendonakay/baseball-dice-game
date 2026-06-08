// Toast notifications, driven by the server via HTMX `HX-Trigger` headers.
//
// A handler returns e.g. `HX-Trigger: {"toast":{"message":"Player updated"}}`
// and HTMX fires a `toast` event on the body. `showToast` renders a transient
// notification — the server decides *when* to notify, the client owns the *how*.

const CONTAINER_ID = "toast-container";
const VISIBLE_MS = 2600;

const container = (): HTMLElement => {
  const existing = document.getElementById(CONTAINER_ID);
  if (existing) return existing;
  const el = document.createElement("div");
  el.id = CONTAINER_ID;
  el.className = "toast-container";
  document.body.appendChild(el);
  return el;
};

export const showToast = (message: string): void => {
  if (!message) return;
  const toast = document.createElement("div");
  toast.className = "toast";
  toast.textContent = message;
  container().appendChild(toast);

  // Force a frame so the entry transition runs.
  requestAnimationFrame(() => toast.classList.add("toast-visible"));

  window.setTimeout(() => {
    toast.classList.remove("toast-visible");
    toast.addEventListener("transitionend", () => toast.remove(), { once: true });
  }, VISIBLE_MS);
};

// Shape of the detail HTMX attaches when the trigger value is an object.
interface ToastDetail {
  message?: string;
}

export const registerToastListener = (): void => {
  document.body.addEventListener("toast", (event: Event) => {
    const detail = (event as CustomEvent<ToastDetail>).detail;
    showToast(detail?.message ?? "");
  });
};
