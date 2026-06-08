# Baseball Dice Game

This has become a playground for various interests of mine. Currently it is a:
- Formally verified, using Lean;
- Baseball simulation;
- Card game;
- Of which, the rules were originally inspired be this [dice
game](https://milb.bamcontent.com/documents/8/5/8/313285858/BaseballDiceGame_LouisvilleBats.pdf).

What I am calling: Waxball!

The game logic has diverged from its original dice game
imeplementation. I'd like to add more realistic properties in the simulation,
but I'm also not going to go for pure realism. I want this to be a fun card
game.

## AI Usage Policy

I currently use Claude Code to assist in the research and development of this
project. At the time of writing I am strictly using the Opus 4.6 model with the
Claude Code CLI tool.

I try to write the README and other human-readable content myself, but if I'm
being honest, I'll probably have Claude do some Mermaid diagrams or something.

## Did you say "Formally verified" at the top there?

I have a deep interest in functional programming and mathematics. As such, I've
been learning the programming language and theorem prover, Lean 4. It may seem
excessive, but I am using this project as an opportunity to play with formal
verification on programs that I am familiar with.

Currently, for this project I am using Lean to formally verify the game state,
functions that operate on that state, and testing parity using an approach
called *Differential Random Testing (DRT)*.

### Differential Random Testing

The formal proofs verify properties of a Lean *model* of the game logic. That
model is a separate thing from the Haskell implementation, which means it is
possible for the two to silently diverge. DRT closes that gap by making the
Lean model *executable* and using it as a "reference oracle" against the running
Haskell implementation.

The flow looks like this:

```
QuickCheck generates a random GameState
        │
        ├───────────────────────────────┐
        ▼                               ▼
Haskell function              Lean executable (JSON over stdin/stdout)
e.g. addBall gs               e.g. DiffTest add-ball
        │                               │
        └──────────────┬────────────────┘
                       ▼
              compare outputs — any divergence
              is a counterexample printed by hspec
```

The Lean executable (`lean/DiffTest/Main.lean`) reads a JSON-encoded
`GameState` from stdin, applies the named pure transition, and writes the
result back as JSON. The Haskell driver (`test/DiffTest.hs`) generates 100
random states per transition, feeds them through both sides, and fails the
test on the first disagreement.

#### Building

The Lean oracle is built through the Nix flake:

```bash
nix build .#difftest
```

This produces `result/bin/DiffTest`. The test suite picks it up automatically.

#### Running

```bash
cabal test spec
```

The DRT tests are included in the existing hspec suite. If `result/bin/DiffTest`
is not present the differential tests are silently skipped — the rest of the
QuickCheck suite still runs.

For more detail on what is proved, the proof techniques, and the design
decisions behind the Lean model, see the [Formal Verification Research](#formal-verification-research)
section below.

## TODO
### App

- [ ] Logging and debug
- [ ] Explicit exports from modules for cleaner code
- [ ] Formalize more of the GameState functions and clean up unused game logic.
  Can go hand in hand with the above.
- [ ] Persistence. Postgres or something.

### Game

Each Game module should live in `src/Game/`

- [ ] Card logic
  - Rip wax
- [ ] Roster
- [ ] Simulator Something to simulate entire seasons/games
- [ ] League
- [ ] Game
  - Player errors

### UI

- [ ] Convert the user dashboard into a banner. This should show up on
      all views. Maybe just do HTMX Gets in a div for detail views of banner
      items.
- [ ] Card design. What JavaScript libraries can be used for this?
- [x] Break out CSS and JS into their own files. CSS in `static/css/app.css`,
      TypeScript islands in `ts/` built by esbuild to `static/js/app.js`, served
      from `/static`. HTMX vendored locally.

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
- [ ] Formal verification of core game logic (Lean 4).
  - [ ] Add Pitcher proofs: model `homePitcher`/`awayPitcher` in the Lean
        `GameState` and prove the count/out/scoring transitions preserve them
        (field isolation). The differential oracle currently echoes these fields
        back verbatim from the input as a stop-gap so the round-trip stays a
        complete `GameState`; replace that with model-serialized pitchers once
        proven.

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
