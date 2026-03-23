-- BaseballVerify.Model
--
-- Pure data types mirroring the Haskell ADTs in src/WaxBall/Game.hs.
-- No IO, no JSON, no monad stack — only the shapes we need to state theorems.
--
-- Haskell reference:
--   data Player      (Game.hs:54)
--   data HalfInning  (Game.hs:108)
--   data BasesState  (Game.hs:137)
--   data GameState   (Game.hs:115)
--   data StrikeAction (Game.hs:515)

namespace BaseballVerify

-- Mirrors Haskell's Player record.
-- onBasePercentage is omitted; it is not used in any state transition we verify.
structure Player where
  name           : String
  number         : Nat
  battingAverage : Nat   -- scaled ×1000: 150–400 represents 0.150–0.400
  sluggingPct    : Nat   -- scaled ×1000: 300–700 represents 0.300–0.700
deriving Repr, DecidableEq

-- Mirrors Haskell's HalfInning (Top | Bottom).
inductive HalfInning
  | Top
  | Bottom
deriving Repr, DecidableEq

-- Mirrors Haskell's BasesState record.
-- `home` is the transient slot: a player in it will score on the next checkScore call.
structure BasesState where
  first  : Option Player
  second : Option Player
  third  : Option Player
  home   : Option Player
deriving Repr, DecidableEq

-- Mirrors Haskell's StrikeAction sum type.
inductive StrikeAction
  | FieldingError
  | FlyOut
  | GroundOut
  | HitByPitch
  | HitDouble
  | HitSingle
  | HitTriple
  | HomeRun
  | PopOut
  | CalledStrike
  | NoAction
deriving Repr, DecidableEq

-- Mirrors Haskell's GameState record.
-- Uses Nat (not Int) for all counts: non-negativity is enforced by the type,
-- which makes lower-bound proofs trivial and eliminates an entire class of bugs.
-- pitchLog is omitted; it is not involved in any state transition we verify.
structure GameState where
  inning        : Nat          -- 1–9+
  halfInning    : HalfInning
  homeScore     : Nat
  awayScore     : Nat
  outs          : Nat          -- 0–2 before triggering half-inning change at 3
  balls         : Nat          -- 0–3 before triggering walk at 4
  strikes       : Nat          -- 0–2 before triggering strikeout at 3
  bases         : BasesState
  currentBatter : Option Player
  homeBatting   : List Player
  awayBatting   : List Player
deriving Repr, DecidableEq

-- Canonical empty bases (no runners).
def emptyBases : BasesState :=
  { first := none, second := none, third := none, home := none }

-- A minimal initial game state useful for constructing proof examples.
def newGameState : GameState :=
  { inning        := 1
    halfInning    := .Top
    homeScore     := 0
    awayScore     := 0
    outs          := 0
    balls         := 0
    strikes       := 0
    bases         := emptyBases
    currentBatter := none
    homeBatting   := []
    awayBatting   := [] }

end BaseballVerify
