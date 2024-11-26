module Main where

import Control.Monad.State
import Game

main :: IO ()
main = do
  let initialGS = initialGameState
  printGameState initialGS
  let ((), finalState) = runState simulateInning initialGS
  printGameState finalState

-- Stupid simple simulation for the sake of running an inning.
simulateInning :: Game ()
simulateInning = do
  batterToFirst
  addOut
  checkOuts
  batterToFirst
  addRun True
  addOut
  checkOuts

-- Simulate game of baseball dice game
simulateGame :: Game ()
simulateGame = do

-- Inning 1
-- First pitch
--  Repeat for 3 outs
