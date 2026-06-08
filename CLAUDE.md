# Baseball Dice Game - Development Notes

## Build System

- Always use `cabal` instead of `stack` for building this project
- Build command: `cabal build`
- Run command: `cabal run`

## Project Structure

- Haskell-based web application using Servant
- Game logic in `src/Game/`
- API handlers in `src/API/`
- Web UI using HTMX and Blaze HTML

## Frontend (CSS / TypeScript)

- Static assets live in `static/` and are served at `/static` via a Servant
  `Raw` route (`serveDirectoryWebApp "static"` in `src/API/Routes.hs`).
- CSS is a plain stylesheet: `static/css/app.css` (palette hoisted into `:root`
  custom properties). No inline `A.style` strings in views — use semantic
  classes. The one exception is the season progress-bar width, which is a
  computed value.
- HTMX is vendored locally at `static/vendor/htmx.min.js` (no CDN).
- Client behavior is small functional TypeScript "islands" in `ts/`, bundled by
  esbuild to `static/js/app.js`. A behavior is `(el) => void` attached to any
  element with `data-behavior="<name>"`; islands are re-scanned on `htmx:load`.
- Prefer server-driven HTMX over client JS: `HX-Redirect` for navigation and
  `HX-Trigger` (e.g. the `toast` event) for notifications, set as response
  headers in handlers — not inline `<script>` tags.
- Frontend build: `npm install` then `npm run build` (typechecks + bundles).
  `npm run dev` watches. `static/js/app.js` is gitignored (build artifact).
- Full build from clean: `npm install && npm run build && cabal build`.

## Player Configuration Feature

- Configuration page available at `/config`
- Allows editing player stats before starting game
- Stats are validated within realistic ranges:
  - Batting Average: 0.150-0.400
  - Slugging Percentage: 0.300-0.700

## Formatting

- Run the Ormolu formatter once a task is completed

## Formal Verification (Lean 4)

- Lean project lives in `lean/` subdirectory
- Build with `lake build` inside `lean/`
- Approved verification plans are stored in `verification-plans/`
- Plans are numbered sequentially: 01-..., 02-..., etc.
- Before starting any verification task: create a plan, get approval, save to verification-plans/
- NeoVim with the Lean 4 LSP is the development environment (not VS Code)
- All formal verification work is on the `lean-formal-verification` branch

