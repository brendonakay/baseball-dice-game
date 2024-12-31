# Baseball Dice Game

Simulated baseball [dice game](https://milb.bamcontent.com/documents/8/5/8/313285858/BaseballDiceGame_LouisvilleBats.pdf).

## TODO
### UI
- [ ] Terminal UI (TUI) using [ `brick`
](https://github.com/jtdaugherty/brick/).
- [ ] HTMX web app.

### Misc
- [ ] Use lenses for game state operations.
  - [ ] Use `aeson` for marshalling data structures into JSON. For sending via a
    backend web API.
- [ ] Reformat `src/` layout. Modularize GameState operators.

## Product Roadmap
My goal with this project is to make a "Diceball-like" game. Where it has some
betting, similar to the original Diceball (craps variant). Except, there's no
real betting. Instead, there's a budget and a payroll. At the start of a season,
you're given a budget. Then, each game eats into your budget via the payroll.
Each player has stats, and those stats are used for each pitch / plate
appearance.

## Architectural Roadmap
- [ ] CLI game.
- [ ] HTMX Web App.
- [ ] Cloud hosted. Lambdas used.

## Misc Thoughts
I think game engine for processing pitches should be:
 - Pitch is thrown. Still use the random coin toss for ball/strike.
   - A better pitching engine could be a future roadmap item.
 - If it is a strike, use the player's batting average to determine a hit. A
 certain modifier will be used to make things more interesting. Perhaps `BA +
 (BA * BA)`.
 - A hit means the ball is in play. Now I need to figure out a way for
 determining the play results. Maybe start with the same set of Strike Actions.
 But tweak them a little bit depending on no-hit strikes and hits.
