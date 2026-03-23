-- DiffTest.Main
--
-- Lean executable for differential random testing.
-- Reads JSON-encoded GameState lines from stdin, applies a named pure
-- transition, and writes the resulting JSON to stdout (one line per input).
--
-- Usage (one subcommand per invocation, all input on stdin):
--   echo '<json>' | ./DiffTest add-ball
--   echo '<json>' | ./DiffTest is-game-over   -- prints "true" or "false"
--
-- The Lean binary is invoked once per command by the Haskell test driver;
-- it processes all test inputs in a single subprocess invocation (streaming).

import BaseballVerify.Model
import BaseballVerify.Transitions
import BaseballVerify.Verify
import Lean.Data.Json

open BaseballVerify Lean

-- ---------------------------------------------------------------------------
-- Streaming helpers
-- ---------------------------------------------------------------------------

-- Process each non-empty stdin line with f; write one JSON result per line.
partial def processLines (f : Json → Except String Json) : IO UInt32 := do
  let stdin  ← IO.getStdin
  let stdout ← IO.getStdout
  let line ← stdin.getLine
  if line.isEmpty then return 0
  let trimmed := line.trim
  if !trimmed.isEmpty then
    match Json.parse trimmed with
    | .error e =>
      stdout.putStrLn s!"\{\"error\": \"parse error: {e}\"}"
      stdout.flush
    | .ok j =>
      match f j with
      | .error e =>
        stdout.putStrLn s!"\{\"error\": \"{e}\"}"
        stdout.flush
      | .ok result =>
        stdout.putStrLn result.compress
        stdout.flush
  processLines f

-- Process each non-empty stdin line; write one plain string result per line.
partial def processLinesStr (f : Json → Except String String) : IO UInt32 := do
  let stdin  ← IO.getStdin
  let stdout ← IO.getStdout
  let line ← stdin.getLine
  if line.isEmpty then return 0
  let trimmed := line.trim
  if !trimmed.isEmpty then
    match Json.parse trimmed with
    | .error e =>
      stdout.putStrLn s!"error: parse error: {e}"
      stdout.flush
    | .ok j =>
      match f j with
      | .error e =>
        stdout.putStrLn s!"error: {e}"
        stdout.flush
      | .ok result =>
        stdout.putStrLn result
        stdout.flush
  processLinesStr f

-- ---------------------------------------------------------------------------
-- Transition wrappers
-- ---------------------------------------------------------------------------

def applyTransition (fn : GameState → GameState) (j : Json) : Except String Json := do
  let gs ← parseGameState j
  .ok (serializeGameState (fn gs))

def applyAddRun (hi : HalfInning) (j : Json) : Except String Json := do
  let gs ← parseGameState j
  .ok (serializeGameState (addRun hi gs))

def applyIsGameOver (j : Json) : Except String String := do
  let gs ← parseGameState j
  .ok (if isGameOver gs then "true" else "false")

def applyPitchBallOrStrike (j : Json) : Except String String := do
  let n ← (j.getObjVal? "n").bind (·.getNat?)
  .ok (match pitchBallOrStrike n with
       | .Ball   => "Ball"
       | .Strike => "Strike")

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

def main (args : List String) : IO UInt32 :=
  match args with
  | ["add-ball"]           => processLines (applyTransition addBall)
  | ["add-strike"]         => processLines (applyTransition addStrike)
  | ["add-out"]            => processLines (applyTransition addOut)
  | ["clear-balls"]        => processLines (applyTransition clearBalls)
  | ["clear-strikes"]      => processLines (applyTransition clearStrikes)
  | ["add-run-top"]        => processLines (applyAddRun .Top)
  | ["add-run-bottom"]     => processLines (applyAddRun .Bottom)
  | ["run-home-run"]       => processLines (applyTransition runHomeRun)
  | ["run-hit-triple"]     => processLines (applyTransition runHitTriple)
  | ["run-hit-double"]     => processLines (applyTransition runHitDouble)
  | ["run-hit-single"]     => processLines (applyTransition runHitSingle)
  | ["is-game-over"]       => processLinesStr applyIsGameOver
  | ["pitch-ball-or-strike"] => processLinesStr applyPitchBallOrStrike
  | _ => do
      (← IO.getStderr).putStrLn
        "Usage: DiffTest <command>\n\
         Commands: add-ball add-strike add-out clear-balls clear-strikes\n\
                   add-run-top add-run-bottom run-home-run run-hit-triple\n\
                   run-hit-double run-hit-single is-game-over pitch-ball-or-strike"
      return 1
