module WaxBall.Dice
  ( rollDiceNTimes,
  )
where

import System.Random (randomRIO)

-- Simulate multiple dice rolls
rollDiceNTimes :: Int -> IO [Int]
rollDiceNTimes n = mapM (const $ randomRIO (1, 6)) [1 .. n]
