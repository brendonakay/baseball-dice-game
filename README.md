# Baseball Dice Game

Simulated baseball [dice game](https://milb.bamcontent.com/documents/8/5/8/313285858/BaseballDiceGame_LouisvilleBats.pdf).

## TODO

- [ ] Create an architectural diagram.
- [ ] Rename the project to baseball card game? I was thinking "Waxball".

### App

- [ ] App context. Gets initialized in `main`
- [ ] Logging and debug
- [ ] Better Auth

### Game

Each Game module should live in `src/Game/`

- [ ] Card logic
  - Rip wax
- [ ] Roster
- [ ] Simulator Something to simulate entire seasons/games
- [ ] Authentication
- [ ] Authorization
- [ ] League
- [ ] Game
  - Errors
  - Pitching

### UI

- [ ] Convert the user dashboard into a banner. This should show up on
      all views. Maybe just do HTMX Gets in a div for detail views of banner
      items.
- [ ] Card design. What JavaScript libraries can be used for this?
- [ ] Break out CSS and JS into their own files.

### API

- [ ] Modularize the API. Break it down so it's not one overloaded Routes
      file.
- [ ] Leverage cool Servant features. API docs?

### Misc

- [ ] Hot reloading. GHCid?
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

## Scripts

### Database Initialization

Initialize a new SQLite database with a users table:

```bash
cabal run init-db -- app.db
```

This creates a database file with a `users` table containing:

- `userID` (INTEGER PRIMARY KEY AUTOINCREMENT)
- `password` (TEXT NOT NULL)
- `created_at` (DATETIME DEFAULT CURRENT_TIMESTAMP)
