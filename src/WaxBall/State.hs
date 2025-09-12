module WaxBall.State
  ( GameRef,
    advanceGameState,
    getCurrentGameState,
  )
where

import Control.Monad.State (runStateT)
import Data.IORef
import WaxBall.Dice (rollDiceNTimes)
import WaxBall.Game (GameState (..), StrikeAction, pitchBallOrStrike, runPitch)

-- Thread-safe reference to game state
type GameRef = IORef GameState

-- Advance the game state by one pitch
advanceGameState :: GameRef -> IO (StrikeAction, GameState)
advanceGameState gameRef = do
  diceRoll <- rollDiceNTimes 3
  currentState <- readIORef gameRef
  (strikeAction, newState) <-
    runStateT
      ( runPitch
          (pitchBallOrStrike (head diceRoll))
          (diceRoll !! 1)
          (diceRoll !! 2)
      )
      currentState
  writeIORef gameRef newState
  return (strikeAction, newState)

-- Get current game state (read-only)
getCurrentGameState :: GameRef -> IO GameState
getCurrentGameState = readIORef
