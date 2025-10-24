# Baseball Dice Game

Simulated baseball [dice game](https://milb.bamcontent.com/documents/8/5/8/313285858/BaseballDiceGame_LouisvilleBats.pdf).

Soon to be: Waxball!

## TODO

- [ ] Create an architectural diagram.
- [ ] Rename the project to baseball card game? I was thinking "Waxball".

### App

- [ ] Logging and debug

### Game

Each Game module should live in `src/Game/`

- [ ] Card logic
  - Rip wax
- [ ] Roster
- [ ] Simulator Something to simulate entire seasons/games
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
- [ ] CI
- [ ] Pre-commit hooks?

## Architectural Roadmap

- [x] HTMX Web App.
- [ ] Cloud hosted.
- [x] Authentication / user base.
- [x] SQLite for persistence.

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

## Util

### Database Utility

The `util` tool provides database initialization, migration, and status commands:

#### Initialize a fresh database

```bash
cabal run util -- db-init app.db
```

#### Apply pending migrations to existing database

```bash
cabal run util -- db-migrate app.db
```

#### Check database status and pending migrations

```bash
cabal run util -- db-status app.db
```

#### Migration System

- Migrations are stored in the `migrations/` directory
- Each migration file follows the naming pattern: `XXX_description.sql`
- The database schema version is tracked using SQLite's `PRAGMA user_version`
- Migrations are applied in numerical order and only once
