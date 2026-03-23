-- BaseballVerify.Proofs.GameTermination
--
-- Tier 5 theorems: game termination conditions.
--
-- isGameOver encodes two win conditions:
--   1. Bottom of inning ≥ 9, home team winning → home wins (walk-off)
--   2. Top of inning ≥ 9, away team winning → away wins after home can't respond
--
-- Notable modeling observation (Part 6 of the plan):
--   In Game.hs lines ~301-325, both the tie-game extra-inning branch and the
--   normal-continuation branch produce identical state transitions. The Lean
--   model makes this ambiguity explicit — nextHalfInningFromBottom covers both
--   cases with a single definition, which is provably correct.

import BaseballVerify.Transitions

namespace BaseballVerify

-- ---------------------------------------------------------------------------
-- isGameOver — necessary conditions
-- ---------------------------------------------------------------------------

-- If the game is over, the inning must be ≥ 9.
theorem gameOver_requires_late_inning (gs : GameState)
    (h : isGameOver gs = true) : gs.inning ≥ 9 := by
  simp [isGameOver] at h
  omega

-- If the game is over, neither team has a tie (there is a winner).
-- Equivalently: homeScore ≠ awayScore.
theorem gameOver_requires_winner (gs : GameState)
    (h : isGameOver gs = true) : gs.homeScore ≠ gs.awayScore := by
  simp [isGameOver] at h
  omega

-- ---------------------------------------------------------------------------
-- isGameOver — sufficient conditions (early innings)
-- ---------------------------------------------------------------------------

-- If inning < 9, the game cannot be over regardless of score.
theorem not_gameOver_early_innings (gs : GameState) (h : gs.inning < 9) :
    isGameOver gs = false := by
  simp [isGameOver]
  omega

-- ---------------------------------------------------------------------------
-- nextHalfInning transitions
-- ---------------------------------------------------------------------------

-- Top → Bottom always sets halfInning to Bottom.
theorem nextHalfInningFromTop_produces_bottom (gs : GameState) :
    (nextHalfInningFromTop gs).halfInning = .Bottom := by
  simp [nextHalfInningFromTop]

-- Top → Bottom always resets outs to 0.
theorem nextHalfInningFromTop_resets_outs (gs : GameState) :
    (nextHalfInningFromTop gs).outs = 0 := by
  simp [nextHalfInningFromTop]

-- Top → Bottom always resets balls to 0.
theorem nextHalfInningFromTop_resets_balls (gs : GameState) :
    (nextHalfInningFromTop gs).balls = 0 := by
  simp [nextHalfInningFromTop]

-- Top → Bottom clears all bases.
theorem nextHalfInningFromTop_clears_bases (gs : GameState) :
    (nextHalfInningFromTop gs).bases = emptyBases := by
  simp [nextHalfInningFromTop]

-- Top → Bottom does not change the inning number.
theorem nextHalfInningFromTop_preserves_inning (gs : GameState) :
    (nextHalfInningFromTop gs).inning = gs.inning := by
  simp [nextHalfInningFromTop]

-- Bottom → Top always sets halfInning to Top.
theorem nextHalfInningFromBottom_produces_top (gs : GameState) :
    (nextHalfInningFromBottom gs).halfInning = .Top := by
  simp [nextHalfInningFromBottom]

-- Bottom → Top always increments the inning.
theorem nextHalfInningFromBottom_increments_inning (gs : GameState) :
    (nextHalfInningFromBottom gs).inning = gs.inning + 1 := by
  simp [nextHalfInningFromBottom]

-- Bottom → Top always resets outs to 0.
theorem nextHalfInningFromBottom_resets_outs (gs : GameState) :
    (nextHalfInningFromBottom gs).outs = 0 := by
  simp [nextHalfInningFromBottom]

-- Bottom → Top clears all bases.
theorem nextHalfInningFromBottom_clears_bases (gs : GameState) :
    (nextHalfInningFromBottom gs).bases = emptyBases := by
  simp [nextHalfInningFromBottom]

-- ---------------------------------------------------------------------------
-- Scores are never modified by half-inning transitions
-- ---------------------------------------------------------------------------

theorem nextHalfInningFromTop_preserves_scores (gs : GameState) :
    (nextHalfInningFromTop gs).homeScore = gs.homeScore ∧
    (nextHalfInningFromTop gs).awayScore = gs.awayScore := by
  simp [nextHalfInningFromTop]

theorem nextHalfInningFromBottom_preserves_scores (gs : GameState) :
    (nextHalfInningFromBottom gs).homeScore = gs.homeScore ∧
    (nextHalfInningFromBottom gs).awayScore = gs.awayScore := by
  simp [nextHalfInningFromBottom]

end BaseballVerify
