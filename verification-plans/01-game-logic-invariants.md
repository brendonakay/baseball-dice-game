# Plan: Formally Verify Baseball Dice Game Using Lean 4

## Context

The baseball dice game is a Haskell/Servant web app with probabilistic game logic. The user wants to begin formal verification using Lean 4 as a learning exercise. The goal is to use Lean to mathematically prove correctness of the game's deterministic rules — things like "after a home run, all bases are cleared" or "a game cannot end before inning 9" — which testing alone cannot guarantee.

This plan establishes the Lean project structure, scope, and a prioritized theorem roadmap appropriate for a beginner.

---

## Key Insight: What We Can (and Cannot) Verify

**Can verify (deterministic logic):**
- Count invariants: balls always 0–3, strikes 0–2, outs 0–2 before transition
- Base state transitions: single/double/triple/HR produce correct base occupancy
- Scoring: home run scores exactly 1 + (number of occupied bases)
- Game end logic: `isGameOver` requires inning ≥ 9 and a score difference

**Cannot verify (requires IO/randomness):**
- `rollDiceNTimes`, `getPlayerStrikeAction`, `determineHitType`, `determineOutType`
- The `StateT GameState IO` monad stack itself
- Correspondence between Lean model and Haskell code (separate artifacts)

**Strategy:** Model only the pure, deterministic state transitions in Lean (as `GameState -> GameState` functions). These correspond 1:1 with the Haskell `modify`/`get`/`put` calls once `IO` is stripped away.

---

## Critical Files (Haskell Source — Read-Only Reference)

- `src/WaxBall/Game.hs` — All core state transitions and data types to model
- `src/WaxBall/State.hs` — Shows the IO boundary; confirms what's out of scope
- `src/WaxBall/Season.hs` — Secondary targets for later (season-level proofs)

---

## Part 1: Key Concepts for Beginners

### Formal Verification vs. Testing
- Tests check finitely many inputs. Proofs cover all possible inputs, forever.
- Lean combines a functional programming language with a theorem prover in one tool.
- **Curry-Howard correspondence**: types are propositions, terms are proofs.
  - `A -> B` is simultaneously "a function from A to B" and "a proof that A implies B"

### Lean 4 Proof Mechanics
- You state a **theorem** (a proposition to prove)
- You prove it interactively using **tactics** — commands that reduce the goal step by step
- The VS Code Lean 4 extension shows the current goal state in a side panel as you type

### Key Resources
1. **Theorem Proving in Lean 4** (TPIL4) — free at `leanprover.github.io/theorem_proving_in_lean4/`
   - Read Chapters 1–4 before writing proofs. Ch. 3 (propositions) and Ch. 4 (quantifiers) are essential.
2. **Functional Programming in Lean** — free at `leanprover.github.io/functional_programming_in_lean/`
   - Read alongside TPIL4 to understand data type definitions
3. **Natural Number Game** — interactive browser tutorial at `adam.math.hhu.de/`
   - Complete Worlds 1–4 to learn `omega`, `simp`, `ring` interactively before Tier 3+ proofs
4. **Lean 4 Zulip** — `leanprover.zulipchat.com` — active, beginner-friendly community
5. **Mathlib docs** — `leanprover-community.github.io/mathlib4_docs/` — search for lemmas on `Nat`, `Option`, `List`

### Tactic Primer (7 tactics you need to start)
| Tactic | Purpose |
|--------|---------|
| `simp [f]` | Unfold definition `f` and simplify equalities |
| `omega` | Solve linear arithmetic over `Nat`/`Int` automatically |
| `rfl` | Close `a = a` goals |
| `exact h` | Close goal using hypothesis `h` |
| `rcases h with _ \| _` | Case-split on `Or` or `Option` |
| `constructor` | Split `And` goals into two subgoals |
| `sorry` | Admit any goal — use while sketching structure |

---

## Part 2: Project Setup

### Location
```
baseball-dice-game/
  lean/                  ← Lean project lives here
    lakefile.lean
    lean-toolchain
    BaseballVerify/
      ...
```

### Install Lean 4 via Nix (add to flake.nix)

Add `elan` to the `packages` list in `flake.nix` — `elan` is Lean's toolchain manager (like `rustup`, or `ghcup`). It provides the `lean` and `lake` binaries and respects `lean-toolchain` pin files.

```nix
# In flake.nix, add to the packages = with pkgs; [...] list:
elan
```

After editing `flake.nix`:
```bash
nix develop    # re-enters dev shell with elan available
lean --version
lake --version
```

### Create Lake project
```bash
cd /home/brendonakay/Workspace/baseball-dice-game
lake new lean    # creates lean/ subdirectory
cd lean
```

### Add Mathlib (in `lakefile.lean`)
```lean
import Lake
open Lake DSL

package "baseball-verify" where
  name := "baseball-verify"

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"

lean_lib BaseballVerify where
  globs := #[.andSubmodules `BaseballVerify]
```

```bash
lake update   # downloads Mathlib (takes several minutes first time)
lake build
```

### NeoVim Setup
The Lean 4 NeoVim extension and LSP `lake` are already installed. The LSP provides an inline
proof state panel (typically via `nvim-leanline` or similar), which shows the current tactic
goal as you type — functionally equivalent to the VS Code side panel.

Use `:LeanGoal` or whatever keybind is configured to toggle the goal panel while writing proofs.

---

## Part 3: Lean Project File Structure

```
lean/BaseballVerify/
  Model.lean              -- Data types mirroring Haskell ADTs
  Transitions.lean        -- Pure state transition functions
  Proofs/
    CountInvariants.lean  -- Tier 1: balls/strikes/outs
    BaseConstraints.lean  -- Tier 2-3: base occupancy
    ScoringCorrectness.lean -- Tier 4: run scoring
    GameTermination.lean  -- Tier 5: isGameOver
```

---

## Part 4: Data Model in Lean (`Model.lean`)

Mirror the Haskell types as pure Lean structures/inductives — no JSON, no IO:

```lean
structure Player where
  name           : String
  number         : Nat
  battingAverage : Float   -- 0.150–0.400
  sluggingPct    : Float   -- 0.300–0.700
deriving Repr, DecidableEq

inductive HalfInning | Top | Bottom deriving Repr, DecidableEq

structure BasesState where
  first  : Option Player
  second : Option Player
  third  : Option Player
  home   : Option Player   -- runner crossing home plate (transient)
deriving Repr, DecidableEq

inductive StrikeAction
  | FieldingError | FlyOut | GroundOut | HitByPitch
  | HitDouble | HitSingle | HitTriple | HomeRun
  | PopOut | CalledStrike | NoAction
deriving Repr, DecidableEq

structure GameState where
  inning        : Nat          -- 1–9+
  halfInning    : HalfInning
  homeScore     : Nat
  awayScore     : Nat
  outs          : Nat          -- 0–2; 3 triggers half-inning change
  balls         : Nat          -- 0–3; 4 triggers walk
  strikes       : Nat          -- 0–2; 3 triggers out
  bases         : BasesState
  currentBatter : Option Player
  homeBatting   : List Player
  awayBatting   : List Player
deriving Repr, DecidableEq
```

**Why `Nat` instead of `Int`?** Non-negativity is enforced by the type. `Nat` can't go below 0, eliminating an entire class of bugs and making lower-bound proofs trivial.

---

## Part 5: Prioritized Theorem Roadmap

### Tier 1 — Count Invariants (Start Here, ~1 afternoon)

Haskell source: `addBall`, `addStrike`, `addOut`, `clearBalls`, `clearStrikes` in `Game.hs`

```lean
-- addBall_le_four: if balls ≤ 3, then after adding one ball, balls ≤ 4
theorem addBall_le_four (gs : GameState) (h : gs.balls ≤ 3) :
    (addBall gs).balls ≤ 4 := by simp [addBall]; omega

-- clearBalls_zero: clearBalls always resets balls to 0
theorem clearBalls_zero (gs : GameState) :
    (clearBalls gs).balls = 0 := by simp [clearBalls]

-- pitchBallOrStrike_alternates: Ball/Strike strictly alternates
theorem pitchBallOrStrike_alternates (n : Nat) :
    pitchBallOrStrike n ≠ pitchBallOrStrike (n + 1) := by
  simp [pitchBallOrStrike]; omega
```

Repeat pattern for `addStrike`, `addOut`, `clearStrikes`.

### Tier 2 — Fields Unmodified By Transitions

Each count-modifying function should not touch other fields:

```lean
theorem addBall_preserves_outs (gs : GameState) :
    (addBall gs).outs = gs.outs := by simp [addBall]
```

Repeat for each combination (addBall/addStrike/addOut × outs/strikes/balls/scores).

### Tier 3 — Base Occupancy After Hit Actions

Haskell source: `runHomeRun`, `runHitTriple`, `runHitDouble`, `runHitSingle` in `Game.hs`

```lean
-- homeRun_clears_bases: after HR, no one is on any base
theorem homeRun_clears_bases (gs : GameState) :
    let gs' := runHomeRun gs
    gs'.bases.first = none ∧ gs'.bases.second = none ∧
    gs'.bases.third = none ∧ gs'.bases.home   = none := by
  simp [runHomeRun, emptyBases]

-- triple_batter_on_third: current batter ends up on 3rd
theorem triple_batter_on_third (gs : GameState) (p : Player)
    (h : gs.currentBatter = some p) :
    (runHitTriple gs).bases.third = some p := by
  simp [runHitTriple, h]

-- triple_first_empty / triple_second_empty
theorem triple_first_empty (gs : GameState) :
    (runHitTriple gs).bases.first = none := by simp [runHitTriple]
```

### Tier 4 — Scoring Correctness

Haskell source: `runHomeRun`, `addRun` in `Game.hs`

```lean
-- addRun .Top increments awayScore by 1, leaves homeScore unchanged
theorem addRun_top_increments_away (gs : GameState) :
    (addRun .Top gs).awayScore = gs.awayScore + 1 := by simp [addRun]

-- homeRun scores exactly 1 + occupiedCount bases
def occupiedCount (b : BasesState) : Nat :=
  (if b.first.isSome  then 1 else 0) +
  (if b.second.isSome then 1 else 0) +
  (if b.third.isSome  then 1 else 0)

theorem homeRun_scores_correctly_top (gs : GameState)
    (h : gs.halfInning = .Top) :
    (runHomeRun gs).awayScore = gs.awayScore + 1 + occupiedCount gs.bases := by
  simp [runHomeRun, occupiedCount, h, addRun]
  rcases gs.bases.first with _ | _ <;>
  rcases gs.bases.second with _ | _ <;>
  rcases gs.bases.third with _ | _ <;> simp; omega
```

### Tier 5 — Game Termination Conditions

Haskell source: `isGameOver`, `nextHalfInning` in `Game.hs`

```lean
-- not_gameOver_early_innings: if inning < 9, game cannot be over
theorem not_gameOver_early_innings (gs : GameState) (h : gs.inning < 9) :
    isGameOver gs = false := by simp [isGameOver]; omega

-- gameOver_requires_late_inning
theorem gameOver_requires_late_inning (gs : GameState)
    (h : isGameOver gs = true) : gs.inning ≥ 9 := by
  simp [isGameOver] at h; omega

-- nextHalfInning .Top always produces .Bottom
theorem nextHalfInning_top_produces_bottom (gs : GameState) :
    (nextHalfInningFromTop gs).halfInning = .Bottom := by
  simp [nextHalfInningFromTop]

-- nextHalfInning resets outs to 0
theorem nextHalfInning_resets_outs (gs : GameState) :
    (nextHalfInningFromTop gs).outs = 0 := by simp [nextHalfInningFromTop]
```

---

## Part 6: Notable Bug Found During Modeling

When modeling `nextHalfInning` from `Game.hs` lines ~301–325, both the tie-game extra-inning branch and the normal-continue branch produce **identical** state transitions. The case for "away team winning at end of an inning ≥ 9 while batting Bottom" may not be handled. The Lean model will surface this ambiguity clearly — this is a real benefit of formal specification.

---

## Part 7: Proof Priority Summary

| Priority | Theorem | Haskell Function | Tactics | Level |
|----------|---------|-----------------|---------|-------|
| 1–5 | Count invariants (balls/strikes/outs) | addBall/addStrike/addOut | simp, omega | Beginner |
| 6–8 | clearBalls/clearStrikes zero | clearBalls/clearStrikes | simp | Beginner |
| 9 | pitchBallOrStrike alternates | pitchBallOrStrike | simp, omega | Beginner |
| 10–15 | Transition field preservation | all count fns | simp | Beginner |
| 16–18 | homeRun clears bases | runHomeRun | simp, rcases | Intermediate |
| 19–21 | triple/double base placement | runHitTriple/Double | simp | Intermediate |
| 22–24 | addRun increments correct score | addRun | simp | Beginner |
| 25 | homeRun scoring count | runHomeRun | simp, rcases, omega | Intermediate |
| 26–27 | isGameOver early inning | isGameOver | simp, omega | Beginner |
| 28 | isGameOver requires winner | isGameOver | simp, rcases, omega | Intermediate |
| 29–30 | nextHalfInning transitions | nextHalfInning | simp | Beginner |
