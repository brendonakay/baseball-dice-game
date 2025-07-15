module Game.State
  ( GameRef,
    initializeGameState,
    advanceGameState,
    getCurrentGameState,
    resetGameState,
  )
where

import Control.Monad.State (runStateT)
import Data.IORef
import Game.Dice (rollDiceNTimes)
import Game.Logic (GameState (..), Player (..), StrikeAction, initialGameState, newGameState, pitchBallOrStrike, runPitch)

-- Thread-safe reference to game state
type GameRef = IORef GameState

-- Initialize a new game with predefined teams
initializeGameState :: IO GameRef
initializeGameState = do
  let homeTeam =
        [ Player {name = "Home A", number = 1},
          Player {name = "Home B", number = 2},
          Player {name = "Home C", number = 3},
          Player {name = "Home D", number = 4},
          Player {name = "Home E", number = 5},
          Player {name = "Home F", number = 6},
          Player {name = "Home G", number = 7},
          Player {name = "Home H", number = 8},
          Player {name = "Home I", number = 9}
        ]
  let awayTeam =
        [ Player {name = "Away A", number = 1},
          Player {name = "Away B", number = 2},
          Player {name = "Away C", number = 3},
          Player {name = "Away D", number = 4},
          Player {name = "Away E", number = 5},
          Player {name = "Away F", number = 6},
          Player {name = "Away G", number = 7},
          Player {name = "Away H", number = 8},
          Player {name = "Away I", number = 9}
        ]

  -- Initialize game state with teams
  (_, initialState) <- runStateT (initialGameState homeTeam awayTeam) newGameState
  newIORef initialState

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

-- Reset game to initial state
resetGameState :: GameRef -> IO ()
resetGameState gameRef = do
  newRef <- initializeGameState
  newState <- getCurrentGameState newRef
  writeIORef gameRef newState
