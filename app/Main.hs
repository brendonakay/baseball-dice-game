{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified API.Routes as App
import Data.IORef (newIORef)
import Network.Wai.Handler.Warp (run)
import WaxBall.Season (newSeasonState)

main :: IO ()
main = do
  -- Initialize empty season state
  putStrLn "=== Initializing Baseball Season ==="
  let emptySeasonState = newSeasonState [] [] -- Start with empty teams
  seasonRef <- newIORef emptySeasonState
  putStrLn "Season initialized and ready to start!"

  -- Start web server with season state
  let port = 8080
  putStrLn $ "Starting server on port " ++ show port
  putStrLn "Visit http://localhost:8080 to start a new season"
  run port (App.app seasonRef)
