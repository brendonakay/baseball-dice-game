module Game.State
  ( GameRef,
    initializeGameState,
    advanceGameState,
    getCurrentGameState,
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
        [ Player
            { name = "Home A",
              number = 1,
              battingAverage = 0.285,
              onBasePercentage = 0.350,
              sluggingPercentage = 0.450
            },
          Player
            { name = "Home B",
              number = 2,
              battingAverage = 0.312,
              onBasePercentage = 0.380,
              sluggingPercentage = 0.520
            },
          Player
            { name = "Home C",
              number = 3,
              battingAverage = 0.267,
              onBasePercentage = 0.330,
              sluggingPercentage =
                0.425
            },
          Player
            { name = "Home D",
              number = 4,
              battingAverage = 0.298,
              onBasePercentage = 0.375,
              sluggingPercentage = 0.580
            },
          Player
            { name = "Home E",
              number = 5,
              battingAverage = 0.245,
              onBasePercentage = 0.315,
              sluggingPercentage = 0.390
            },
          Player
            { name = "Home F",
              number = 6,
              battingAverage = 0.278,
              onBasePercentage = 0.340,
              sluggingPercentage =
                0.465
            },
          Player
            { name = "Home G",
              number = 7,
              battingAverage = 0.292,
              onBasePercentage = 0.360,
              sluggingPercentage = 0.485
            },
          Player
            { name = "Home H",
              number = 8,
              battingAverage = 0.255,
              onBasePercentage = 0.325,
              sluggingPercentage = 0.410
            },
          Player
            { name = "Home I",
              number = 9,
              battingAverage = 0.220,
              onBasePercentage = 0.280,
              sluggingPercentage =
                0.340
            }
        ]
  let awayTeam =
        [ Player
            { name = "Away A",
              number = 1,
              battingAverage = 0.275,
              onBasePercentage = 0.345,
              sluggingPercentage = 0.440
            },
          Player
            { name = "Away B",
              number = 2,
              battingAverage = 0.305,
              onBasePercentage = 0.370,
              sluggingPercentage = 0.510
            },
          Player
            { name = "Away C",
              number = 3,
              battingAverage = 0.258,
              onBasePercentage = 0.320,
              sluggingPercentage =
                0.415
            },
          Player
            { name = "Away D",
              number = 4,
              battingAverage = 0.289,
              onBasePercentage = 0.365,
              sluggingPercentage = 0.565
            },
          Player
            { name = "Away E",
              number = 5,
              battingAverage = 0.235,
              onBasePercentage = 0.305,
              sluggingPercentage = 0.380
            },
          Player
            { name = "Away F",
              number = 6,
              battingAverage = 0.270,
              onBasePercentage = 0.335,
              sluggingPercentage =
                0.455
            },
          Player
            { name = "Away G",
              number = 7,
              battingAverage = 0.284,
              onBasePercentage = 0.355,
              sluggingPercentage = 0.475
            },
          Player
            { name = "Away H",
              number = 8,
              battingAverage = 0.248,
              onBasePercentage = 0.318,
              sluggingPercentage = 0.400
            },
          Player
            { name = "Away I",
              number = 9,
              battingAverage = 0.210,
              onBasePercentage = 0.270,
              sluggingPercentage = 0.320
            }
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
