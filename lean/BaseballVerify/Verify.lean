-- BaseballVerify.Verify
--
-- JSON serialization / deserialization for the Lean model types.
-- Uses Lean.Data.Json (built into Lean 4 core — no Mathlib/Batteries needed).
--
-- Wire format matches Haskell's Aeson generic derivation exactly so that
-- the DiffTest executable can round-trip through Haskell's FromJSON/ToJSON.
--
-- JsonNumber representation: mantissa × 10^(-exponent)
--   Integer 5  → ⟨5, 0⟩   (5 × 10^0 = 5)
--   Float 0.312 → ⟨312, 3⟩ (312 × 10^(-3) = 0.312)
--
-- Field-name mapping notes (Lean model → JSON wire name):
--   Player.battingAverage : Nat (×1000) ↔ "battingAverage" : Float
--   Player.sluggingPct    : Nat (×1000) ↔ "sluggingPercentage" : Float
--   onBasePercentage is not in the Lean model; serialized as 0
--   pitchLog is not in the Lean model; serialized as []

import BaseballVerify.Model
import Lean.Data.Json

namespace BaseballVerify

open Lean

-- ---------------------------------------------------------------------------
-- Field access helpers (thin wrappers around the built-in Json API)
-- ---------------------------------------------------------------------------

private def getField (j : Json) (key : String) : Except String Json :=
  j.getObjVal? key

private def getStr (j : Json) (key : String) : Except String String := do
  (← getField j key).getStr?

-- Extract a Nat from a JSON number field.
-- Safe for non-negative integer values (mantissa.toNat is 0 for negatives).
private def getNat (j : Json) (key : String) : Except String Nat := do
  let v ← getField j key
  match v with
  | .num n => .ok n.mantissa.toNat
  | _      => .error s!"field '{key}' is not a number"

-- Extract a Float from a JSON number field.
private def getFloat (j : Json) (key : String) : Except String Float := do
  let v ← getField j key
  match v with
  | .num n => .ok n.toFloat
  | _      => .error s!"field '{key}' is not a number"

-- Return None if the key is absent or its value is null; Some v otherwise.
private def getOptional (j : Json) (key : String) : Except String (Option Json) :=
  match j.getObjVal? key with
  | .ok .null => .ok none
  | .ok v     => .ok (some v)
  | .error _  => .ok none  -- missing key treated as None

-- ---------------------------------------------------------------------------
-- HalfInning
-- ---------------------------------------------------------------------------

def serializeHalfInning : HalfInning → Json
  | .Top    => Json.str "Top"
  | .Bottom => Json.str "Bottom"

def parseHalfInning (j : Json) : Except String HalfInning := do
  let s ← j.getStr?
  match s with
  | "Top"    => .ok .Top
  | "Bottom" => .ok .Bottom
  | _        => .error s!"expected 'Top' or 'Bottom', got {s}"

-- ---------------------------------------------------------------------------
-- Player
-- ---------------------------------------------------------------------------

-- Serialize Nat (stored as thousandths) as a JSON decimal float.
-- e.g., battingAverage = 312 → Json.num ⟨312, 3⟩ → renders as "0.312"
private def natThousandthsToJson (n : Nat) : Json :=
  Json.num ⟨n, 3⟩

def serializePlayer (p : Player) : Json :=
  Json.mkObj
    [ ("name",               Json.str p.name)
    , ("number",             Json.num (JsonNumber.fromNat p.number))
    , ("battingAverage",     natThousandthsToJson p.battingAverage)
    , ("onBasePercentage",   Json.num (JsonNumber.fromNat 0))
    , ("sluggingPercentage", natThousandthsToJson p.sluggingPct)
    ]

def parsePlayer (j : Json) : Except String Player := do
  let n   ← getStr   j "name"
  let num ← getNat   j "number"
  let ba  ← getFloat j "battingAverage"
  let sp  ← getFloat j "sluggingPercentage"
  .ok { name           := n
        number         := num
        battingAverage := (ba * 1000.0).round.toUInt64.toNat
        sluggingPct    := (sp * 1000.0).round.toUInt64.toNat }

-- ---------------------------------------------------------------------------
-- BasesState
-- ---------------------------------------------------------------------------

def serializeBasesState (b : BasesState) : Json :=
  let slot : Option Player → Json
    | none   => Json.null
    | some p => serializePlayer p
  Json.mkObj
    [ ("first",  slot b.first)
    , ("second", slot b.second)
    , ("third",  slot b.third)
    , ("home",   slot b.home)
    ]

def parseBasesState (j : Json) : Except String BasesState := do
  let parseSlot (key : String) : Except String (Option Player) := do
    match ← getOptional j key with
    | none   => .ok none
    | some v => some <$> parsePlayer v
  let f ← parseSlot "first"
  let s ← parseSlot "second"
  let t ← parseSlot "third"
  let h ← parseSlot "home"
  .ok { first := f, second := s, third := t, home := h }

-- ---------------------------------------------------------------------------
-- Player list helpers
-- ---------------------------------------------------------------------------

private def serializePlayerList (ps : List Player) : Json :=
  Json.arr (ps.map serializePlayer).toArray

private def parsePlayerList (j : Json) : Except String (List Player) := do
  let elems ← j.getArr?
  elems.toList.mapM parsePlayer

-- ---------------------------------------------------------------------------
-- GameState
-- ---------------------------------------------------------------------------

-- The JSON fields the Lean model tracks. Factored out so we can serialize a
-- GameState either on its own or with additional preserved fields appended.
private def gameStateFields (gs : GameState) : List (String × Json) :=
  let cb : Json := match gs.currentBatter with
    | none   => Json.null
    | some p => serializePlayer p
  [ ("inning",        Json.num (JsonNumber.fromNat gs.inning))
  , ("halfInning",    serializeHalfInning gs.halfInning)
  , ("homeBatting",   serializePlayerList gs.homeBatting)
  , ("awayBatting",   serializePlayerList gs.awayBatting)
  , ("homeScore",     Json.num (JsonNumber.fromNat gs.homeScore))
  , ("awayScore",     Json.num (JsonNumber.fromNat gs.awayScore))
  , ("outs",          Json.num (JsonNumber.fromNat gs.outs))
  , ("balls",         Json.num (JsonNumber.fromNat gs.balls))
  , ("strikes",       Json.num (JsonNumber.fromNat gs.strikes))
  , ("bases",         serializeBasesState gs.bases)
  , ("currentBatter", cb)
  , ("pitchLog",      Json.arr #[])
  ]

def serializeGameState (gs : GameState) : Json :=
  Json.mkObj (gameStateFields gs)

-- Serialize a GameState, appending extra raw JSON fields verbatim. Used by the
-- differential oracle to echo back fields the Lean model does not track (e.g.
-- homePitcher/awayPitcher) so the round-trip stays a complete GameState.
def serializeGameStateWith (extra : List (String × Json)) (gs : GameState) : Json :=
  Json.mkObj (gameStateFields gs ++ extra)

def parseGameState (j : Json) : Except String GameState := do
  let inn ← getNat j "inning"
  let hi  ← parseHalfInning (← getField j "halfInning")
  let hb  ← parsePlayerList (← getField j "homeBatting")
  let ab  ← parsePlayerList (← getField j "awayBatting")
  let hs  ← getNat j "homeScore"
  let as' ← getNat j "awayScore"
  let o   ← getNat j "outs"
  let b   ← getNat j "balls"
  let s   ← getNat j "strikes"
  let bs  ← parseBasesState (← getField j "bases")
  let cb  ← do
    match ← getOptional j "currentBatter" with
    | none   => pure none
    | some v => some <$> parsePlayer v
  .ok { inning        := inn
        halfInning    := hi
        homeBatting   := hb
        awayBatting   := ab
        homeScore     := hs
        awayScore     := as'
        outs          := o
        balls         := b
        strikes       := s
        bases         := bs
        currentBatter := cb }

end BaseballVerify
