{-# LANGUAGE OverloadedStrings #-}

module User.Seed where

import Control.Monad (unless)
import Data.Text (Text)
import Database.SQLite.Simple

-- Seeded player roster with realistic stats.
-- era = Nothing for position players; Just x for pitchers / two-way players.
seededPlayers :: [(Text, Int, Text, Double, Double, Double, Maybe Double)]
seededPlayers =
  [ ("Mike Trout", 27, "Angels", 0.301, 0.418, 0.587, Nothing),
    ("Mookie Betts", 50, "Dodgers", 0.307, 0.392, 0.579, Nothing),
    ("Ronald Acuña Jr.", 13, "Braves", 0.337, 0.416, 0.596, Nothing),
    ("Juan Soto", 22, "Padres", 0.275, 0.410, 0.519, Nothing),
    ("Aaron Judge", 99, "Yankees", 0.267, 0.373, 0.648, Nothing),
    ("Shohei Ohtani", 17, "Angels", 0.304, 0.412, 0.654, Nothing),
    ("Trea Turner", 6, "Phillies", 0.298, 0.358, 0.466, Nothing),
    ("Vladimir Guerrero Jr.", 27, "Blue Jays", 0.281, 0.362, 0.480, Nothing),
    ("Fernando Tatis Jr.", 23, "Padres", 0.257, 0.341, 0.540, Nothing),
    ("Francisco Lindor", 12, "Mets", 0.270, 0.354, 0.449, Nothing),
    ("Manny Machado", 13, "Padres", 0.278, 0.356, 0.471, Nothing),
    ("Jose Altuve", 27, "Astros", 0.288, 0.354, 0.476, Nothing),
    ("Freddie Freeman", 5, "Dodgers", 0.331, 0.410, 0.567, Nothing),
    ("Pete Alonso", 20, "Mets", 0.271, 0.352, 0.547, Nothing),
    ("Bo Bichette", 11, "Blue Jays", 0.306, 0.349, 0.516, Nothing),
    ("Corey Seager", 5, "Rangers", 0.290, 0.352, 0.521, Nothing),
    ("Kyle Tucker", 30, "Astros", 0.284, 0.362, 0.537, Nothing),
    ("Jazz Chisholm Jr.", 2, "Marlins", 0.254, 0.327, 0.535, Nothing),
    -- Pitchers (era IS NOT NULL — used as season pitchers, not batters)
    ("Gerrit Cole", 45, "Yankees", 0.113, 0.147, 0.145, Just 2.84),
    ("Sandy Alcantara", 22, "Marlins", 0.107, 0.138, 0.134, Just 2.28)
  ]

-- Idempotently seed players and the "1960 Classic Series" set
seedDatabase :: Connection -> IO ()
seedDatabase conn = do
  addEraColumnIfMissing conn
  execute_ conn "INSERT OR IGNORE INTO sets (name) VALUES ('1960 Classic Series')"
  mapM_ (insertPlayerIfMissing conn) seededPlayers

-- Add the era column to an existing players table if it is not already present.
-- Needed for databases created before the era column was added to the schema.
addEraColumnIfMissing :: Connection -> IO ()
addEraColumnIfMissing conn = do
  cols <-
    query_ conn "PRAGMA table_info(players)" ::
      IO [(Int, Text, Text, Int, Maybe Text, Int)]
  unless (any (\(_, colName, _, _, _, _) -> colName == "era") cols) $
    execute_ conn "ALTER TABLE players ADD COLUMN era REAL"

insertPlayerIfMissing :: Connection -> (Text, Int, Text, Double, Double, Double, Maybe Double) -> IO ()
insertPlayerIfMissing conn (playerName, playerNum, _team, avg, obp, slg, playerEra) = do
  existing <-
    query
      conn
      "SELECT id FROM players WHERE name = ?"
      (Only playerName) ::
      IO [Only Int]
  case existing of
    [] ->
      execute
        conn
        "INSERT INTO players (name, number, batting_average, on_base_percentage, slugging_percentage, era) VALUES (?, ?, ?, ?, ?, ?)"
        (playerName, playerNum, avg, obp, slg, playerEra)
    _ -> return ()

-- Assign 9 Base cards from the seeded roster to a new user
assignStarterPack :: Connection -> Int -> IO ()
assignStarterPack conn userId = do
  setRows <-
    query_
      conn
      "SELECT id FROM sets WHERE name = '1960 Classic Series'" ::
      IO [Only Int]
  case setRows of
    [] -> return ()
    (Only setId : _) -> do
      let first9 = take 9 seededPlayers
      mapM_ (assignCard conn userId setId) (zip [1 ..] first9)

assignCard :: Connection -> Int -> Int -> (Int, (Text, Int, Text, Double, Double, Double, Maybe Double)) -> IO ()
assignCard conn userId setId (idx, (playerName, _pnum, team, _avg, _obp, _slg, _era)) = do
  playerRows <-
    query
      conn
      "SELECT id FROM players WHERE name = ?"
      (Only playerName) ::
      IO [Only Int]
  case playerRows of
    (Only playerId : _) -> do
      let cardNum = "1960-" <> playerName
      execute
        conn
        "INSERT INTO cards (number, player_id, set_id, user_id, team, card_class) VALUES (?, ?, ?, ?, ?, ?)"
        (cardNum, playerId :: Int, setId :: Int, userId :: Int, team, "BASE" :: Text)
      return ()
    _ -> return ()
  return $ idx `seq` ()
