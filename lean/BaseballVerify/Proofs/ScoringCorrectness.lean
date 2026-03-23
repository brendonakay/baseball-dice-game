-- BaseballVerify.Proofs.ScoringCorrectness
--
-- Tier 4 theorems: run scoring correctness.
--
-- Key theorem: a home run scores exactly 1 (the batter) plus the number of
-- runners already on base. Proved by exhaustive case-splitting on which bases
-- are occupied (8 cases via rcases).

import BaseballVerify.Transitions

namespace BaseballVerify

-- ---------------------------------------------------------------------------
-- addRun
-- ---------------------------------------------------------------------------

-- addRun .Top increments the away score by 1.
theorem addRun_top_increments_away (gs : GameState) :
    (addRun .Top gs).awayScore = gs.awayScore + 1 := by
  simp [addRun]

-- addRun .Top leaves the home score unchanged.
theorem addRun_top_preserves_home (gs : GameState) :
    (addRun .Top gs).homeScore = gs.homeScore := by
  simp [addRun]

-- addRun .Bottom increments the home score by 1.
theorem addRun_bottom_increments_home (gs : GameState) :
    (addRun .Bottom gs).homeScore = gs.homeScore + 1 := by
  simp [addRun]

-- addRun .Bottom leaves the away score unchanged.
theorem addRun_bottom_preserves_away (gs : GameState) :
    (addRun .Bottom gs).awayScore = gs.awayScore := by
  simp [addRun]

-- ---------------------------------------------------------------------------
-- Home run scoring (Tier 4 key theorem)
-- ---------------------------------------------------------------------------

-- A home run in the Top half scores exactly (1 + occupiedCount) runs for
-- the away team, and leaves the home score unchanged.
--
-- Proof strategy: case-split on whether each of the three bases is occupied
-- (Some vs None), then let simp + omega close the arithmetic for each case.
theorem homeRun_scores_correctly_top (gs : GameState)
    (h : gs.halfInning = .Top) :
    (runHomeRun gs).awayScore = gs.awayScore + 1 + occupiedCount gs.bases := by
  simp [runHomeRun, occupiedCount, h]
  cases gs.bases.first <;>
  cases gs.bases.second <;>
  cases gs.bases.third <;>
  simp <;> omega

-- Home run in the Bottom half: symmetric — home score increases by run count.
theorem homeRun_scores_correctly_bottom (gs : GameState)
    (h : gs.halfInning = .Bottom) :
    (runHomeRun gs).homeScore = gs.homeScore + 1 + occupiedCount gs.bases := by
  simp [runHomeRun, occupiedCount, h]
  cases gs.bases.first <;>
  cases gs.bases.second <;>
  cases gs.bases.third <;>
  simp <;> omega

-- A home run in the Top half leaves the home score untouched.
theorem homeRun_top_preserves_homeScore (gs : GameState)
    (h : gs.halfInning = .Top) :
    (runHomeRun gs).homeScore = gs.homeScore := by
  simp [runHomeRun, h]

-- A home run in the Bottom half leaves the away score untouched.
theorem homeRun_bottom_preserves_awayScore (gs : GameState)
    (h : gs.halfInning = .Bottom) :
    (runHomeRun gs).awayScore = gs.awayScore := by
  simp [runHomeRun, h]

-- occupiedCount is always between 0 and 3.
theorem occupiedCount_le_three (b : BasesState) :
    occupiedCount b ≤ 3 := by
  simp [occupiedCount]
  cases b.first <;>
  cases b.second <;>
  cases b.third <;>
  simp <;> omega

end BaseballVerify
