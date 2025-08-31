{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified API.Routes as App
import Game.State (initializeGameState)
import Network.Wai.Handler.Warp (run)

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
  run port (App.app gameRef)
