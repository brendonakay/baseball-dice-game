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