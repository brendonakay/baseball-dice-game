{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified API.Routes as App
import Control.Monad.State
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as B
import Game.Dice
import Game.Logic
import Game.State (initializeGameState)
import Network.Wai.Handler.Warp (run)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import UI.HTMX
import UI.TUI
import Web.Scotty as S

main :: IO ()
main = do
  -- Initialize game state
  putStrLn "=== Initializing Baseball Game ==="
  gameRef <- initializeGameState
  putStrLn "Game initialized with teams and ready to play!"

  -- Start web server with game state
  let port = 8080
  putStrLn $ "Starting server on port " ++ show port
  putStrLn "Visit http://localhost:8080 to view the game"
  putStrLn "Each request to /data will advance the game by one pitch"
  run port (App.app gameRef)

-- main :: IO ()
-- main = do
--   -- Initialize teams
--   let homeTeam =
--         [ Player {name = "A", number = 01},
--           Player {name = "B", number = 02},
--           Player {name = "C", number = 03},
--           Player {name = "D", number = 04},
--           Player {name = "E", number = 05},
--           Player {name = "F", number = 06},
--           Player {name = "G", number = 07},
--           Player {name = "H", number = 08},
--           Player {name = "I", number = 09}
--         ]
--   let awayTeam =
--         [ Player {name = "A", number = 01},
--           Player {name = "B", number = 02},
--           Player {name = "C", number = 03},
--           Player {name = "D", number = 04},
--           Player {name = "E", number = 05},
--           Player {name = "F", number = 06},
--           Player {name = "G", number = 07},
--           Player {name = "H", number = 08},
--           Player {name = "I", number = 09}
--         ]
--
--   -- Play ball!
--   let (_, initialGS) = runState (initialGameState homeTeam awayTeam) newGameState
--
--   -- Simulate text game
--   printGameState initialGS
--   runAndPrintGame initialGS

-- Run Game TUI version
-- runGameTUI initialGS homeTeam awayTeam

runGameTUI :: GameState -> HomeTeam -> AwayTeam -> IO ()
runGameTUI gs ht at = do
  diceRoll <- rollDiceNTimes 3
  (_, nextState) <-
    runStateT
      ( runPitch
          (pitchBallOrStrike (head diceRoll))
          (diceRoll !! 1)
          (diceRoll !! 2)
      )
      gs

  if inning nextState <= 9
    then runGameTUI nextState ht at
    else do
      let pl = pitchLog gs
      runGameTableTUI (reverse (pitchLogToString pl))

runAndPrintGame :: GameState -> IO ()
runAndPrintGame gs = do
  diceRoll <- rollDiceNTimes 3
  print $ "Dice roll: " ++ show diceRoll
  (strikeAction, nextState) <-
    runStateT
      ( runPitch
          (pitchBallOrStrike (head diceRoll))
          (diceRoll !! 1)
          (diceRoll !! 2)
      )
      gs

  printGameState nextState
  print strikeAction

  if inning nextState <= 9
    then runAndPrintGame nextState
    else do
      print ""
      print ""
      print "Game over!"
      print ""
      print ""
      print "Game Log: "
      B.putStrLn $ encode $ reverse (pitchLog gs)

printGameState :: GameState -> IO ()
printGameState gs =
  print (show gs)
