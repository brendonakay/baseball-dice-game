-- BaseballVerify.Proofs.BaseConstraints
--
-- Tier 3 theorems: base occupancy after hit actions.
--
-- These proofs verify that runHitTriple, runHitDouble, runHitSingle, and
-- runHomeRun place runners on exactly the correct bases and clear the rest.
--
-- Tactics used: simp, cases, constructor.

import BaseballVerify.Transitions

namespace BaseballVerify

-- ---------------------------------------------------------------------------
-- Home run clears all bases
-- ---------------------------------------------------------------------------

-- After a home run, no runner occupies any base (all four slots are none).
theorem homeRun_clears_bases (gs : GameState) :
    let gs' := runHomeRun gs
    gs'.bases.first = none ∧
    gs'.bases.second = none ∧
    gs'.bases.third = none ∧
    gs'.bases.home = none := by
  simp [runHomeRun, occupiedCount]
  cases gs.halfInning <;> simp [emptyBases]

-- After a home run, balls is reset to 0.
theorem homeRun_clears_balls (gs : GameState) :
    (runHomeRun gs).balls = 0 := by
  simp [runHomeRun, occupiedCount]
  cases gs.halfInning <;> simp

-- After a home run, strikes is reset to 0.
theorem homeRun_clears_strikes (gs : GameState) :
    (runHomeRun gs).strikes = 0 := by
  simp [runHomeRun, occupiedCount]
  cases gs.halfInning <;> simp

-- ---------------------------------------------------------------------------
-- Triple base placement
-- ---------------------------------------------------------------------------

-- After a triple, the batter is on third.
theorem triple_batter_on_third (gs : GameState) (p : Player)
    (h : gs.currentBatter = some p) :
    (runHitTriple gs).bases.third = some p := by
  simp [runHitTriple, h]

-- After a triple, first base is always empty.
theorem triple_first_empty (gs : GameState) :
    (runHitTriple gs).bases.first = none := by
  simp [runHitTriple]

-- After a triple, second base is always empty.
theorem triple_second_empty (gs : GameState) :
    (runHitTriple gs).bases.second = none := by
  simp [runHitTriple]

-- After a triple, the runner who was on first is in the home slot (will score).
theorem triple_first_runner_scores (gs : GameState) :
    (runHitTriple gs).bases.home = gs.bases.first := by
  simp [runHitTriple]

-- ---------------------------------------------------------------------------
-- Double base placement
-- ---------------------------------------------------------------------------

-- After a double, the batter is on second.
theorem double_batter_on_second (gs : GameState) (p : Player)
    (h : gs.currentBatter = some p) :
    (runHitDouble gs).bases.second = some p := by
  simp [runHitDouble, h]

-- After a double, first base is always empty.
theorem double_first_empty (gs : GameState) :
    (runHitDouble gs).bases.first = none := by
  simp [runHitDouble]

-- After a double, the runner who was on first advances to third.
theorem double_first_runner_to_third (gs : GameState) :
    (runHitDouble gs).bases.third = gs.bases.first := by
  simp [runHitDouble]

-- After a double, the runner who was on second is in the home slot (will score).
theorem double_second_runner_scores (gs : GameState) :
    (runHitDouble gs).bases.home = gs.bases.second := by
  simp [runHitDouble]

-- ---------------------------------------------------------------------------
-- Single base placement
-- ---------------------------------------------------------------------------

-- After a single, the batter is on first.
theorem single_batter_on_first (gs : GameState) (p : Player)
    (h : gs.currentBatter = some p) :
    (runHitSingle gs).bases.first = some p := by
  simp [runHitSingle, h]

-- After a single, all runners advance exactly one base.
theorem single_advances_first_to_second (gs : GameState) :
    (runHitSingle gs).bases.second = gs.bases.first := by
  simp [runHitSingle]

theorem single_advances_second_to_third (gs : GameState) :
    (runHitSingle gs).bases.third = gs.bases.second := by
  simp [runHitSingle]

-- The runner on third goes to the home slot (will score).
theorem single_third_runner_scores (gs : GameState) :
    (runHitSingle gs).bases.home = gs.bases.third := by
  simp [runHitSingle]

end BaseballVerify
