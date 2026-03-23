#!/usr/bin/env cabal
{- cabal:
build-depends: base, sqlite-simple, text, directory, filepath, random
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Database utility tool for baseball-dice-game
-- Handles database initialization, migrations, and status checking

module Main where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless, when)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import Database.SQLite.Simple
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (dropExtension, takeExtension, (</>))
import System.Random

-- Migration data type
data Migration = Migration
  { migrationVersion :: Int,
    migrationFile :: FilePath,
    migrationDescription :: Text
  }
  deriving (Show, Eq)

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
        (versionStr, '_' : description) -> do
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
  exists <- doesDirectoryExist migrationsDir
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
  TO.readFile filepath

-- Apply a single migration
applyMigration :: Connection -> Migration -> IO ()
applyMigration conn migration@(Migration version _ description) = do
  putStrLn $ "Applying migration " ++ show version ++ ": " ++ T.unpack description
  content <- readMigrationContent migration
  -- Split content into individual statements and execute each
  let statements = filter (not . T.null . T.strip) $ T.splitOn ";" content
  mapM_ (\stmt -> execute_ conn (Query $ T.strip stmt)) statements
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
    return ()

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

-- Test card generation functions

-- Player data for realistic test cards
testPlayers :: [(Text, Int, Text)] -- (name, number, team)
testPlayers =
  [ ("Mike Trout", 27, "Angels"),
    ("Mookie Betts", 50, "Dodgers"),
    ("Ronald Acuña Jr.", 13, "Braves"),
    ("Juan Soto", 22, "Padres"),
    ("Aaron Judge", 99, "Yankees"),
    ("Shohei Ohtani", 17, "Angels"),
    ("Trea Turner", 6, "Phillies"),
    ("Vladimir Guerrero Jr.", 27, "Blue Jays"),
    ("Fernando Tatis Jr.", 23, "Padres"),
    ("Francisco Lindor", 12, "Mets"),
    ("Manny Machado", 13, "Padres"),
    ("Jose Altuve", 27, "Astros"),
    ("Freddie Freeman", 5, "Dodgers"),
    ("Pete Alonso", 20, "Mets"),
    ("Bo Bichette", 11, "Blue Jays"),
    ("Corey Seager", 5, "Rangers"),
    ("Kyle Tucker", 30, "Astros"),
    ("Jazz Chisholm Jr.", 2, "Marlins")
  ]

-- Generate realistic batting stats
generatePlayerStats :: IO (Double, Double, Double)
generatePlayerStats = do
  -- Batting average between 0.220 and 0.340
  avg <- randomRIO (0.220, 0.340)
  -- On-base percentage typically 30-80 points higher than batting average
  obp <- randomRIO (avg + 0.030, avg + 0.080)
  -- Slugging percentage between 0.350 and 0.650
  slg <- randomRIO (0.350, 0.650)
  return (avg, obp, slg)

-- Card type distribution for 18 cards
-- 12 Base, 3 Parallel, 2 Insert, 1 Serial
cardDistribution :: [Text]
cardDistribution = replicate 12 "BASE" ++ replicate 3 "PARALLEL" ++ replicate 2 "INSERT" ++ ["BASE"]

-- Special card distribution (only for non-base cards)
specialDistribution :: [Maybe Text]
specialDistribution =
  replicate 12 Nothing
    ++ replicate 2 Nothing
    ++ [Just "SERIAL"]
    ++ replicate 2 Nothing
    ++ [Just "AUTOGRAPH"]

-- Generate a random card number
generateCardNumber :: IO Text
generateCardNumber = do
  num <- randomRIO (1, 350) :: IO Int
  return $ T.pack $ show num

-- Insert a player into the database and return the player ID
insertPlayer :: Connection -> (Text, Int, Text) -> (Double, Double, Double) -> IO Int
insertPlayer conn (playerName, playerNum, _) (avg, obp, slg) = do
  execute
    conn
    "INSERT INTO players (name, number, batting_average, on_base_percentage, slugging_percentage) VALUES (?, ?, ?, ?, ?)"
    (playerName, playerNum, avg, obp, slg)
  lastId <- lastInsertRowId conn
  return $ fromIntegral lastId

-- Insert a set into the database and return the set ID
insertSet :: Connection -> Text -> IO Int
insertSet conn setName = do
  execute conn "INSERT INTO sets (name) VALUES (?)" (Only setName)
  lastId <- lastInsertRowId conn
  return $ fromIntegral lastId

-- Insert a card into the database
insertCard :: Connection -> Int -> Int -> Int -> Text -> Text -> Text -> Maybe Text -> IO ()
insertCard conn playerId setId userId cardNum team cardClass special = do
  execute
    conn
    "INSERT INTO cards (number, player_id, set_id, user_id, team, card_class, special) VALUES (?, ?, ?, ?, ?, ?, ?)"
    (cardNum, playerId, setId, userId, team, cardClass, special)

-- Generate all test cards for a user
generateUserPersonalCollection :: String -> Int -> IO ()
generateUserPersonalCollection dbPath userId = do
  putStrLn $ "Generating 18 test cards for user ID: " ++ show userId
  putStrLn $ "Database: " ++ dbPath

  bracket (open dbPath) close $ \conn -> do
    -- Create the test set
    setId <- insertSet conn "2024 Topps Series 1"
    putStrLn $ "✓ Created card set with ID: " ++ show setId

    -- Generate cards with the specified distribution
    let playerData = take 18 $ cycle testPlayers
    let cardTypes = cardDistribution
    let specials = specialDistribution

    mapM_
      ( \(_, ((playerName, playerNum, team), cardType, special)) -> do
          -- Generate player stats
          stats <- generatePlayerStats

          -- Insert player
          playerId <- insertPlayer conn (playerName, playerNum, team) stats

          -- Generate card number
          cardNum <- generateCardNumber

          -- Insert card
          insertCard conn playerId setId userId cardNum team cardType special

          let cardDesc = case special of
                Just s -> T.unpack cardType ++ " (" ++ T.unpack s ++ ")"
                Nothing -> T.unpack cardType

          putStrLn $ "  ✓ " ++ T.unpack playerName ++ " #" ++ T.unpack cardNum ++ " - " ++ cardDesc
      )
      (zip [1 ..] (zip3 playerData cardTypes specials))

    putStrLn $ "✓ Successfully generated 18 test cards!"

    -- Print summary
    putStrLn "\nCard Distribution:"
    putStrLn "  • 12 Base cards"
    putStrLn "  • 3 Parallel cards"
    putStrLn "  • 2 Insert cards"
    putStrLn "  • 1 Serial card"
    putStrLn "  • 1 Autograph card"

-- Print usage information
printUsage :: IO ()
printUsage = do
  putStrLn "Database utility tool for baseball-dice-game"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  cabal run util -- db-init <database_path>    # Initialize fresh database"
  putStrLn "  cabal run util -- db-migrate <database_path> # Apply pending migrations"
  putStrLn "  cabal run util -- db-status <database_path>  # Show migration status"
  putStrLn "  cabal run util -- gen-user-pc <database_path> <user_id> # Generate test cards for user"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  cabal run util -- db-init app.db"
  putStrLn "  cabal run util -- db-migrate app.db"
  putStrLn "  cabal run util -- db-status app.db"
  putStrLn "  cabal run util -- gen-user-pc app.db 1"

-- Main entry point
main :: IO ()
main = do
  args <- getArgs
  result <- try $ case args of
    ["db-init", dbPath] -> initializeDatabase dbPath
    ["db-migrate", dbPath] -> migrateDatabase dbPath
    ["db-status", dbPath] -> showDatabaseStatus dbPath
    ["gen-user-pc", dbPath, userIdStr] -> do
      case reads userIdStr of
        [(userId, "")] -> generateUserPersonalCollection dbPath userId
        _ -> do
          putStrLn $ "Error: Invalid user ID '" ++ userIdStr ++ "'. Must be a number."
          exitFailure
    _ -> do
      printUsage
      exitFailure

  case result of
    Left (ex :: SomeException) -> do
      putStrLn $ "❌ Error: " ++ show ex
      exitFailure
    Right _ -> return ()
