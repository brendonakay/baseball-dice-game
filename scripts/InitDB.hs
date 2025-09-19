#!/usr/bin/env cabal
{- cabal:
build-depends: base, sqlite-simple, text
-}
{-# LANGUAGE OverloadedStrings #-}

-- Simple database initialization script for baseball-dice-game
-- Creates SQLite database with a users table

module Main where

import Control.Exception (bracket)
import Data.Text (Text)
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- Users table schema
createUsersTable :: Query
createUsersTable =
  Query $
    "CREATE TABLE IF NOT EXISTS users (\
    \  userID INTEGER PRIMARY KEY AUTOINCREMENT,\
    \  username TEXT UNIQUE NOT NULL,\
    \  email TEXT UNIQUE NOT NULL,\
    \  password TEXT NOT NULL,\
    \  created_at DATETIME DEFAULT CURRENT_TIMESTAMP\
    \)"

-- Initialize database with users table
initializeDatabase :: String -> IO ()
initializeDatabase dbPath = do
  putStrLn $ "Initializing database at: " ++ dbPath

  bracket (open dbPath) close $ \conn -> do
    putStrLn "Creating users table..."
    execute_ conn createUsersTable
    putStrLn "Database initialization complete!"

-- Print usage information
printUsage :: IO ()
printUsage = do
  putStrLn "Usage: cabal run scripts/InitDB.hs -- <database_path>"
  putStrLn "Example: cabal run scripts/InitDB.hs -- app.db"
  putStrLn "Creates a new SQLite database with a users table."

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dbPath] -> initializeDatabase dbPath
    _ -> do
      printUsage
      exitFailure
