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

