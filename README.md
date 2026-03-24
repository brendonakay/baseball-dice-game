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
  - Collection
- [ ] Roster
- [ ] Simulator Something to simulate entire seasons/games
- [ ] League
- [ ] Game
  - Player errors
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
- [ ] Formal verification of core game logic (Lean 4).

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

## Formal Verification Research

This project uses [Lean 4](https://leanprover.github.io/lean4/doc/) to formally verify the
correctness of the game's deterministic rule engine. This is a learning exercise in applied
formal methods — the goal is to move from "tests pass" to "mathematically proven impossible
to violate."

### Why Lean 4?

Lean 4 is simultaneously a dependently-typed functional programming language and an
interactive theorem prover. It unifies code and proof in a single language via the
**Curry-Howard correspondence**: a type `A → B` is both "a function from A to B" and
"a proof that A implies B." This means the data types modeling the game state and the
theorems about that state live in the same file, checked by the same compiler.

Lean's `Mathlib` library provides a vast collection of reusable lemmas (especially for
`Nat`, `Option`, and `List`) that dramatically reduce proof effort for arithmetic invariants.

### Goal

Prove that the deterministic game rules are correct for **all possible inputs**, not just
a finite set of test cases. A formal proof is a certificate that no edge case can violate
the stated property — forever, regardless of future code changes (until the spec changes).

### Strategy: Pure Core Extraction

The Haskell game logic runs inside `StateT GameState IO`, which mixes pure state transitions
with randomness (`randomRIO`, dice rolls) and IO effects. Formal verification of effectful,
nondeterministic code requires significantly more infrastructure (monadic specifications,
probability theory, or relational models).

The approach taken here is **pure core extraction**:

1. Identify the deterministic `modify $ \gs -> gs { ... }` calls inside each `Game ()` action.
2. Rewrite each as a plain `GameState → GameState` function in Lean, stripping `IO` and
   the monad stack entirely.
3. State and prove theorems about those pure functions.

The resulting Lean functions are not mechanically linked to the Haskell source — they are
a separate formal specification that serves as a ground truth for what the rules *should* do.
Any divergence between Lean and Haskell is a bug in Haskell.

### What Is Verified

| Category | Examples |
|----------|---------|
| **Count invariants** | After `addBall`, `balls` increases by exactly 1; `clearBalls` always resets to 0 |
| **Field isolation** | `addBall` never changes `outs`, `strikes`, or either score |
| **Base transitions** | After a triple: batter on third, first/second empty, old first-base runner scores |
| **Scoring correctness** | Home run scores exactly `1 + occupiedCount` runs for the correct team |
| **Game termination** | `isGameOver` is false if `inning < 9`; game-over requires a winner (no ties) |
| **Half-inning resets** | `nextHalfInning` always resets outs to 0, clears bases, and flips the half |

### What Is Not Verified

Anything that touches `IO` or randomness is out of scope:

- `randomRIO` dice rolls and their distribution
- `getPlayerStrikeAction` — hit/out determination from batting average + dice
- `determineHitType` / `determineOutType` — probabilistic outcome selection
- The `StateT GameState IO` monad stack itself

### Notable Finding During Modeling

When extracting `nextHalfInning` from `Game.hs` lines 300–325, the tie-game extra-inning
branch and the normal-continue branch produce **identical** state transitions. The case for
"away team winning at end of an inning ≥ 9 while batting Bottom" appears unreachable or
unhandled. The Lean model makes this ambiguity structurally visible — a direct benefit of
writing a formal specification alongside the implementation.

### Key Proof Techniques

| Tactic | Purpose |
|--------|---------|
| `simp [f]` | Unfold definition `f` and close simple equalities |
| `omega` | Automatically solve linear arithmetic goals over `Nat` |
| `rfl` | Close `a = a` goals |
| `rcases h with _ \| _` | Case-split on `Or`, `Option`, or inductive types |
| `constructor` | Split an `And` goal into two subgoals |

The tiers progress from pure `simp`/`omega` (Tier 1: count invariants) through
`rcases` case-splitting on base occupancy (Tier 3–4) to conditional reasoning
about game termination (Tier 5).

### Design Choice: `Nat` over `Int`

All game counters (balls, strikes, outs, scores, inning) are modeled as `Nat` rather
than `Int`. `Nat` cannot go below zero, so all lower-bound proofs (e.g., "balls ≥ 0")
are trivially true by type, eliminating an entire class of invariants from the proof
obligation.

### Project Structure

```
lean/
  lakefile.lean                      -- Lake build config; declares Mathlib dependency
  lean-toolchain                     -- Pins the exact Lean 4 version
  BaseballVerify.lean                -- Top-level import
  BaseballVerify/
    Model.lean                       -- Data types mirroring Haskell ADTs
    Transitions.lean                 -- Pure GameState → GameState functions
    Proofs/
      CountInvariants.lean           -- Tier 1–2: balls/strikes/outs
      BaseConstraints.lean           -- Tier 3: base occupancy after hits
      ScoringCorrectness.lean        -- Tier 4: run scoring
      GameTermination.lean           -- Tier 5: isGameOver, half-inning resets

verification-plans/
  01-game-logic-invariants.md        -- Approved verification plan (this effort)
```

### Building

```bash
cd lean
lake update   # first time only — downloads Mathlib (several minutes)
lake build
```

### Resources

- [Theorem Proving in Lean 4](https://leanprover.github.io/theorem_proving_in_lean4/) —
  read Chapters 1–4 before writing proofs (Ch. 3 on propositions and Ch. 4 on quantifiers
  are essential)
- [Functional Programming in Lean](https://leanprover.github.io/functional_programming_in_lean/) —
  covers data type definitions and structural recursion
- [Mathlib documentation](https://leanprover-community.github.io/mathlib4_docs/) —
  search for lemmas on `Nat`, `Option`, `List`
- [Natural Number Game](https://adam.math.hhu.de/) — interactive browser tutorial;
  complete Worlds 1–4 to practice `omega`, `simp`, and `ring`
- [Lean 4 Zulip](https://leanprover.zulipchat.com) — active, beginner-friendly community
