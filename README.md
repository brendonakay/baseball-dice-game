# Baseball Dice Game

Simulated baseball [dice game](https://milb.bamcontent.com/documents/8/5/8/313285858/BaseballDiceGame_LouisvilleBats.pdf).

## TODO

- [ ] Create an architectural diagram.
- [ ] Rename the project to baseball card game? I was thinking "Waxball".

### Game

Each Game module should live in `src/Game/`.

- [ ] League
- [ ] Season
- [ ] Cards
- [ ] Logic
  - Errors
  - Pitching

### UI

- [ ] HTMX web app.
- [ ] Card design. What JavaScript libraries can be used for this?

### Misc

- [ ] Hot reloading.
- [ ] Use lenses for game state operations?

## Architectural Roadmap

- [ ] HTMX Web App.
- [ ] Cloud hosted.
- [ ] Authentication / user base.
- [ ] SQLite for persistence.

## User Flow

1. User logs in.
2. User is brought to their landing page.
3. The landing page displays:

   - Team info
   - Season info
   - Card inventory
   - Schedule
   - Market, TBD

4. The landing page links to detailed views of the various components
   listed.
