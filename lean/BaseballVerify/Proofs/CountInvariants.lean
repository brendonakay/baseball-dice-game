-- BaseballVerify.Proofs.CountInvariants
--
-- Tier 1 & 2 theorems: balls, strikes, and outs count invariants.
--
-- These are the first proofs to attempt. Every theorem here uses only
-- `simp` (to unfold definitions) and `omega` (linear arithmetic).
--
-- Proof priority: 1–15 from the roadmap in verification-plans/01-game-logic-invariants.md

import BaseballVerify.Transitions

namespace BaseballVerify

-- ---------------------------------------------------------------------------
-- Tier 1 — addBall
-- ---------------------------------------------------------------------------

-- After adding a ball, the ball count increases by exactly 1.
theorem addBall_increments (gs : GameState) :
    (addBall gs).balls = gs.balls + 1 := by
  simp [addBall]

-- If balls ≤ 3, after addBall the count is still ≤ 4 (walk threshold).
theorem addBall_le_four (gs : GameState) (h : gs.balls ≤ 3) :
    (addBall gs).balls ≤ 4 := by
  simp [addBall]; omega

-- addBall does not change the out count.
theorem addBall_preserves_outs (gs : GameState) :
    (addBall gs).outs = gs.outs := by
  simp [addBall]

-- addBall does not change the strike count.
theorem addBall_preserves_strikes (gs : GameState) :
    (addBall gs).strikes = gs.strikes := by
  simp [addBall]

-- addBall does not change either score.
theorem addBall_preserves_homeScore (gs : GameState) :
    (addBall gs).homeScore = gs.homeScore := by
  simp [addBall]

theorem addBall_preserves_awayScore (gs : GameState) :
    (addBall gs).awayScore = gs.awayScore := by
  simp [addBall]

-- ---------------------------------------------------------------------------
-- Tier 1 — addStrike
-- ---------------------------------------------------------------------------

theorem addStrike_increments (gs : GameState) :
    (addStrike gs).strikes = gs.strikes + 1 := by
  simp [addStrike]

theorem addStrike_le_three (gs : GameState) (h : gs.strikes ≤ 2) :
    (addStrike gs).strikes ≤ 3 := by
  simp [addStrike]; omega

theorem addStrike_preserves_outs (gs : GameState) :
    (addStrike gs).outs = gs.outs := by
  simp [addStrike]

theorem addStrike_preserves_balls (gs : GameState) :
    (addStrike gs).balls = gs.balls := by
  simp [addStrike]

-- ---------------------------------------------------------------------------
-- Tier 1 — addOut
-- ---------------------------------------------------------------------------

theorem addOut_increments (gs : GameState) :
    (addOut gs).outs = gs.outs + 1 := by
  simp [addOut]

theorem addOut_le_three (gs : GameState) (h : gs.outs ≤ 2) :
    (addOut gs).outs ≤ 3 := by
  simp [addOut]; omega

theorem addOut_preserves_balls (gs : GameState) :
    (addOut gs).balls = gs.balls := by
  simp [addOut]

theorem addOut_preserves_strikes (gs : GameState) :
    (addOut gs).strikes = gs.strikes := by
  simp [addOut]

-- ---------------------------------------------------------------------------
-- Tier 1 — clearBalls / clearStrikes
-- ---------------------------------------------------------------------------

theorem clearBalls_zero (gs : GameState) :
    (clearBalls gs).balls = 0 := by
  simp [clearBalls]

theorem clearStrikes_zero (gs : GameState) :
    (clearStrikes gs).strikes = 0 := by
  simp [clearStrikes]

-- clearBalls does not disturb strikes or outs.
theorem clearBalls_preserves_strikes (gs : GameState) :
    (clearBalls gs).strikes = gs.strikes := by
  simp [clearBalls]

theorem clearBalls_preserves_outs (gs : GameState) :
    (clearBalls gs).outs = gs.outs := by
  simp [clearBalls]

-- ---------------------------------------------------------------------------
-- Tier 1 — pitchBallOrStrike alternation
-- ---------------------------------------------------------------------------

-- An even number produces Ball and an odd number produces Strike.
-- Therefore consecutive naturals always produce different results.
theorem pitchBallOrStrike_alternates (n : Nat) :
    pitchBallOrStrike n ≠ pitchBallOrStrike (n + 1) := by
  unfold pitchBallOrStrike
  by_cases h : n % 2 = 0
  · have h1 : (n + 1) % 2 ≠ 0 := by omega
    rw [if_pos h, if_neg h1]
    decide
  · have h1 : (n + 1) % 2 = 0 := by omega
    rw [if_neg h, if_pos h1]
    decide

end BaseballVerify
