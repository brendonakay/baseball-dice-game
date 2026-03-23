-- BaseballVerify.Transitions
--
-- Pure, deterministic state transition functions extracted from the Haskell
-- StateT GameState IO monad stack. Each function here is the "pure core" of
-- its Haskell counterpart — identical logic, but GameState -> GameState
-- instead of Game ().
--
-- Haskell reference (src/WaxBall/Game.hs):
--   addBall      :232   addStrike    :238   addOut       :227
--   clearBalls   :243   clearStrikes :246
--   addRun       :177   checkScore   :338
--   runHomeRun   :491   runHitTriple :473   runHitDouble :451   runHitSingle :468
--   isGameOver   :355   nextHalfInning :288
--   pitchBallOrStrike :63

import BaseballVerify.Model

namespace BaseballVerify

-- ---------------------------------------------------------------------------
-- Pitch type (used only in pitchBallOrStrike)
-- ---------------------------------------------------------------------------

inductive Pitch | Ball | Strike deriving Repr, DecidableEq

-- Mirrors Haskell's pitchBallOrStrike (Game.hs:63).
-- Even n → Ball, odd n → Strike.
def pitchBallOrStrike (n : Nat) : Pitch :=
  if n % 2 = 0 then .Ball else .Strike

-- ---------------------------------------------------------------------------
-- Count modifiers
-- ---------------------------------------------------------------------------

-- Mirrors addBall (Game.hs:232).  Does NOT call checkBalls — keep pure.
def addBall (gs : GameState) : GameState :=
  { gs with balls := gs.balls + 1 }

-- Mirrors addStrike (Game.hs:238).  Does NOT call checkStrikes — keep pure.
def addStrike (gs : GameState) : GameState :=
  { gs with strikes := gs.strikes + 1 }

-- Mirrors addOut (Game.hs:227).  Does NOT call checkOuts — keep pure.
def addOut (gs : GameState) : GameState :=
  { gs with outs := gs.outs + 1 }

-- Mirrors clearBalls (Game.hs:243).
def clearBalls (gs : GameState) : GameState :=
  { gs with balls := 0 }

-- Mirrors clearStrikes (Game.hs:246).
def clearStrikes (gs : GameState) : GameState :=
  { gs with strikes := 0 }

-- ---------------------------------------------------------------------------
-- Scoring
-- ---------------------------------------------------------------------------

-- Mirrors addRun (Game.hs:177).
-- Top half → away team scores; Bottom half → home team scores.
def addRun (hi : HalfInning) (gs : GameState) : GameState :=
  match hi with
  | .Top    => { gs with awayScore := gs.awayScore + 1 }
  | .Bottom => { gs with homeScore := gs.homeScore + 1 }

-- Number of runners occupying first, second, or third.
-- Used in homeRun scoring correctness theorem.
def occupiedCount (b : BasesState) : Nat :=
  (if b.first.isSome  then 1 else 0) +
  (if b.second.isSome then 1 else 0) +
  (if b.third.isSome  then 1 else 0)

-- ---------------------------------------------------------------------------
-- Hit action transitions (pure cores of runHit* from Game.hs)
-- ---------------------------------------------------------------------------

-- Mirrors runHomeRun (Game.hs:491).
-- Clears all bases, scores 1 run per runner + the batter.
-- NOTE: We model the multi-addRun loop as a single addition of occupiedCount+1
-- for the purpose of proving scoring correctness.
def runHomeRun (gs : GameState) : GameState :=
  let b       := gs.bases
  let runCount := 1 + occupiedCount b
  let hi      := gs.halfInning
  let cleared := { gs with bases := emptyBases }
  match hi with
  | .Top    => { cleared with awayScore := cleared.awayScore + runCount
                              strikes   := 0
                              balls     := 0 }
  | .Bottom => { cleared with homeScore := cleared.homeScore + runCount
                              strikes   := 0
                              balls     := 0 }

-- Mirrors runHitTriple (Game.hs:473).
-- Batter goes to third; runner on first scores (goes to home transient slot);
-- runners on second and third are cleared (third was occupied by batter,
-- second had nowhere to advance past third — Haskell drops them).
-- After: first = none, second = none, third = batter, home = old first.
def runHitTriple (gs : GameState) : GameState :=
  let b       := gs.bases
  let newBases : BasesState :=
    { first  := none
      second := none
      third  := gs.currentBatter
      home   := b.first }
  { gs with bases   := newBases
            strikes := 0
            balls   := 0 }

-- Mirrors runHitDouble (Game.hs:451).
-- Batter goes to second; runner on first goes to third;
-- runner on second scores (home slot); runner on third is cleared.
def runHitDouble (gs : GameState) : GameState :=
  let b       := gs.bases
  let newBases : BasesState :=
    { first  := none
      second := gs.currentBatter
      third  := b.first
      home   := b.second }
  { gs with bases   := newBases
            strikes := 0
            balls   := 0 }

-- Mirrors runHitSingle / advanceRunners (Game.hs:183,468).
-- Every runner advances one base; batter goes to first.
def runHitSingle (gs : GameState) : GameState :=
  let b       := gs.bases
  let newBases : BasesState :=
    { first  := gs.currentBatter
      second := b.first
      third  := b.second
      home   := b.third }
  { gs with bases   := newBases
            strikes := 0
            balls   := 0 }

-- ---------------------------------------------------------------------------
-- Half-inning transitions (pure core of nextHalfInning, Game.hs:288)
-- ---------------------------------------------------------------------------

-- Top → Bottom: reset outs/balls/bases, change half-inning.
def nextHalfInningFromTop (gs : GameState) : GameState :=
  { gs with halfInning    := .Bottom
            outs          := 0
            balls         := 0
            bases         := emptyBases
            currentBatter := none }

-- Bottom → Top of next inning: increment inning, reset counts.
-- Models the two identical branches in Haskell (tie and normal continuation).
def nextHalfInningFromBottom (gs : GameState) : GameState :=
  { gs with halfInning    := .Top
            inning        := gs.inning + 1
            outs          := 0
            balls         := 0
            bases         := emptyBases
            currentBatter := none }

-- ---------------------------------------------------------------------------
-- Game-over predicate (mirrors isGameOver, Game.hs:355)
-- ---------------------------------------------------------------------------

-- Returns true when the game has a winner and is in inning ≥ 9.
def isGameOver (gs : GameState) : Bool :=
  let homeWinning := gs.homeScore > gs.awayScore
  let awayWinning := gs.awayScore > gs.homeScore
  (gs.inning ≥ 9 && gs.halfInning == .Bottom && homeWinning) ||
  (gs.inning ≥ 9 && gs.halfInning == .Top    && awayWinning)

end BaseballVerify
