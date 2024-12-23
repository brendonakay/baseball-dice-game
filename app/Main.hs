module Main where

import Control.Monad.State
import Dice
import Game

main :: IO ()
main = do
  -- Initialize teams
  let homeTeam =
        HomeTeam
          [ Player {name = "A", number = 01},
            Player {name = "B", number = 02},
            Player {name = "C", number = 03},
            Player {name = "D", number = 04},
            Player {name = "E", number = 05},
            Player {name = "F", number = 06},
            Player {name = "G", number = 07},
            Player {name = "H", number = 08},
            Player {name = "I", number = 09}
          ]
  let awayTeam =
        AwayTeam
          [ Player {name = "A", number = 01},
            Player {name = "B", number = 02},
            Player {name = "C", number = 03},
            Player {name = "D", number = 04},
            Player {name = "E", number = 05},
            Player {name = "F", number = 06},
            Player {name = "G", number = 07},
            Player {name = "H", number = 08},
            Player {name = "I", number = 09}
          ]

  let initialGS = initialGameState
  printGameState initialGS

  -- Simulate game
  runAndPrintGame initialGS

runAndPrintGame :: GameState -> IO ()
runAndPrintGame gs = do
  diceRoll <- rollDiceNTimes 3
  print $ "Dice roll: " ++ show diceRoll
  let ((), nextState) =
        runState
          ( runPitch
              (pitchBallOrStrike (head diceRoll))
              (diceRoll !! 1)
              (diceRoll !! 2)
          )
          gs

  printGameState nextState

  if inning nextState <= 9
    then runAndPrintGame nextState
    else print "Game over!"

printGameState :: GameState -> IO ()
printGameState gs =
  print (show gs)
