{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified API.Routes as App
import Crypto.JOSE.JWK (KeyMaterialGenParam (RSAGenParam), genJWK)
import Data.IORef (newIORef)
import Database.SQLite.Simple (open)
import Network.Wai.Handler.Warp (run)
import Servant (Context (..))
import Servant.Auth.Server as SAS
import User.Auth ()
import User.AuthenticatedUser (AuthenticatedUser (..))
import WaxBall.Season (newSeasonState)

main :: IO ()
main = do
  -- Initialize database connection
  putStrLn "=== Initializing Baseball Game ==="
  dbConn <- open "app.db"
  putStrLn "Database connected!"

  -- Initialize default user state (will be replaced by authenticated user)
  userRef <- newIORef (Nothing :: Maybe AuthenticatedUser) -- No user initially
  putStrLn "User state initialized!"

  -- Initialize empty season state
  let emptySeasonState = newSeasonState [] [] -- Start with empty teams
  seasonRef <- newIORef emptySeasonState
  putStrLn "Season initialized and ready to start!"

  -- Generate JWT key for authentication
  jwk <- genJWK (RSAGenParam 256)
  putStrLn "JWT key generated!"

  -- Configure JWT and Cookie settings
  let jwtCfg = defaultJWTSettings jwk
      cookieCfg =
        defaultCookieSettings
          { cookieIsSecure = NotSecure, -- For development
            cookieXsrfSetting = Nothing -- Disable XSRF protection for testing
          }
      ctx = cookieCfg :. jwtCfg :. EmptyContext
  putStrLn "Authentication context configured!"

  -- Start web server with database connection, user and season state
  let port = 8080
  putStrLn $ "Starting server on port " ++ show port
  putStrLn "Visit http://localhost:8080 to login"
  run port (App.app ctx dbConn userRef seasonRef)
