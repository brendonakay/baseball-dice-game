{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified API.Routes as App
import Data.IORef (newIORef)
import Network.Wai.Handler.Warp (run)
import User.Account (User (..), UserRef)
import WaxBall.Card (Card (..))
import WaxBall.Game (Player (..))
import WaxBall.Season (newSeasonState)

main :: IO ()
main = do
  -- Initialize default user
  putStrLn "=== Initializing Baseball Game ==="
  let samplePlayers =
        [ Player "Ace Martinez" 1 0.295 0.365 0.520,
          Player "Bobby Smith" 2 0.312 0.380 0.485,
          Player "Carlos Rivera" 3 0.267 0.340 0.445,
          Player "Danny Wilson" 4 0.285 0.355 0.510,
          Player "Eddie Johnson" 5 0.275 0.350 0.460,
          Player "Frank Garcia" 6 0.289 0.370 0.495,
          Player "George Brown" 7 0.301 0.375 0.535,
          Player "Henry Davis" 8 0.258 0.330 0.425,
          Player "Ivan Rodriguez" 9 0.278 0.345 0.470
        ]
      defaultCards = zipWith (\i player -> Card i ("CARD-00" ++ show i) player) [1 .. 9] samplePlayers
      defaultUser = User 1 "Baseball Fan" "fan@baseball.com" defaultCards
  userRef <- newIORef defaultUser
  putStrLn "User initialized!"

  -- Initialize empty season state
  let emptySeasonState = newSeasonState [] [] -- Start with empty teams
  seasonRef <- newIORef emptySeasonState
  putStrLn "Season initialized and ready to start!"

  -- Start web server with user and season state
  let port = 8080
  putStrLn $ "Starting server on port " ++ show port
  putStrLn "Visit http://localhost:8080 to start a new season"
  run port (App.app userRef seasonRef)
