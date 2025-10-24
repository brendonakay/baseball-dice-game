#!/usr/bin/env cabal
{- cabal:
build-depends: base, sqlite-simple, text, directory, filepath
-}
{-# LANGUAGE OverloadedStrings #-}

-- Database utility tool for baseball-dice-game
-- Handles database initialization, migrations, and status checking

module Main where

import Control.Exception (bracket, try, SomeException)
import Control.Monad (when, unless)
import Database.SQLite.Simple
import Data.List (sort, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, listDirectory, createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeExtension, dropExtension)

-- Migration data type
data Migration = Migration
  { migrationVersion :: Int
  , migrationFile :: FilePath
  , migrationDescription :: Text
  } deriving (Show, Eq)

instance Ord Migration where
  compare (Migration v1 _ _) (Migration v2 _ _) = compare v1 v2

-- Get current database schema version
getCurrentVersion :: Connection -> IO Int
getCurrentVersion conn = do
  result <- query_ conn "PRAGMA user_version" :: IO [Only Int]
  case result of
    [Only version] -> return version
    _ -> return 0

-- Set database schema version
setVersion :: Connection -> Int -> IO ()
setVersion conn version = do
  execute_ conn $ Query $ "PRAGMA user_version = " <> T.pack (show version)

-- Parse migration filename to extract version and description
parseMigrationFile :: FilePath -> Maybe Migration
parseMigrationFile filename
  | takeExtension filename == ".sql" = do
      let baseName = dropExtension filename
      case break (== '_') baseName of
        (versionStr, '_':description) -> do
          version <- readMaybe versionStr
          return $ Migration version filename (T.pack description)
        _ -> Nothing
  | otherwise = Nothing
  where
    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

-- Discover all migration files in migrations directory
discoverMigrations :: IO [Migration]
discoverMigrations = do
  let migrationsDir = "migrations"
  createDirectoryIfMissing False migrationsDir
  exists <- doesFileExist migrationsDir
  if exists
    then do
      files <- listDirectory migrationsDir
      let migrations = [m | f <- files, Just m <- [parseMigrationFile f]]
      return $ sort migrations
    else return []

-- Read migration file content
readMigrationContent :: Migration -> IO Text
readMigrationContent (Migration _ filename _) = do
  let filepath = "migrations" </> filename
  TIO.readFile filepath

-- Apply a single migration
applyMigration :: Connection -> Migration -> IO ()
applyMigration conn migration@(Migration version _ description) = do
  putStrLn $ "Applying migration " ++ show version ++ ": " ++ T.unpack description
  content <- readMigrationContent migration
  execute_ conn (Query content)
  setVersion conn version
  putStrLn $ "✓ Migration " ++ show version ++ " applied successfully"

-- Get pending migrations (those with version > current database version)
getPendingMigrations :: Connection -> [Migration] -> IO [Migration]
getPendingMigrations conn allMigrations = do
  currentVersion <- getCurrentVersion conn
  return $ filter (\(Migration v _ _) -> v > currentVersion) allMigrations

-- Initialize fresh database with all migrations
initializeDatabase :: String -> IO ()
initializeDatabase dbPath = do
  putStrLn $ "Initializing database at: " ++ dbPath
  
  migrations <- discoverMigrations
  when (null migrations) $ do
    putStrLn "Warning: No migrations found in migrations/ directory"
    
  bracket (open dbPath) close $ \conn -> do
    mapM_ (applyMigration conn) migrations
    putStrLn "✓ Database initialization complete!"

-- Apply pending migrations to existing database
migrateDatabase :: String -> IO ()
migrateDatabase dbPath = do
  putStrLn $ "Migrating database at: " ++ dbPath
  
  exists <- doesFileExist dbPath
  unless exists $ do
    putStrLn $ "Error: Database file " ++ dbPath ++ " does not exist"
    putStrLn "Use 'db-init' to create a new database"
    exitFailure
    
  migrations <- discoverMigrations
  bracket (open dbPath) close $ \conn -> do
    pending <- getPendingMigrations conn migrations
    if null pending
      then putStrLn "✓ Database is up to date, no migrations needed"
      else do
        putStrLn $ "Found " ++ show (length pending) ++ " pending migration(s)"
        mapM_ (applyMigration conn) pending
        putStrLn "✓ Database migration complete!"

-- Show database status and pending migrations
showDatabaseStatus :: String -> IO ()
showDatabaseStatus dbPath = do
  putStrLn $ "Database status for: " ++ dbPath
  
  exists <- doesFileExist dbPath
  unless exists $ do
    putStrLn "❌ Database file does not exist"
    return
    
  migrations <- discoverMigrations
  bracket (open dbPath) close $ \conn -> do
    currentVersion <- getCurrentVersion conn
    pending <- getPendingMigrations conn migrations
    
    putStrLn $ "Current schema version: " ++ show currentVersion
    putStrLn $ "Available migrations: " ++ show (length migrations)
    putStrLn $ "Pending migrations: " ++ show (length pending)
    
    unless (null pending) $ do
      putStrLn "\nPending migrations:"
      mapM_ (\(Migration v _ desc) -> putStrLn $ "  " ++ show v ++ ": " ++ T.unpack desc) pending

-- Print usage information
printUsage :: IO ()
printUsage = do
  putStrLn "Database utility tool for baseball-dice-game"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  cabal run util -- db-init <database_path>    # Initialize fresh database"
  putStrLn "  cabal run util -- db-migrate <database_path> # Apply pending migrations"
  putStrLn "  cabal run util -- db-status <database_path>  # Show migration status"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  cabal run util -- db-init app.db"
  putStrLn "  cabal run util -- db-migrate app.db"
  putStrLn "  cabal run util -- db-status app.db"

-- Main entry point
main :: IO ()
main = do
  args <- getArgs
  result <- try $ case args of
    ["db-init", dbPath] -> initializeDatabase dbPath
    ["db-migrate", dbPath] -> migrateDatabase dbPath
    ["db-status", dbPath] -> showDatabaseStatus dbPath
    _ -> do
      printUsage
      exitFailure
  
  case result of
    Left (ex :: SomeException) -> do
      putStrLn $ "❌ Error: " ++ show ex
      exitFailure
    Right _ -> return ()