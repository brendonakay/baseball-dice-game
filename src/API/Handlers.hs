module API.Handlers where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Game.Logic
  ( BasesState (..),
    GameState (..),
    Log (..),
    Player (..),
    StrikeAction (..),
    isGameOver,
  )
import Game.State (GameRef, advanceGameState, getCurrentGameState, initializeGameStateWithTeams)
import Data.IORef (writeIORef)
import qualified Data.List as List
import Text.Read (readMaybe)
import Servant
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx as Htmx

-- Get current game state and render as HTML row
getGameDataRow :: GameRef -> Handler Html
getGameDataRow gameRef = do
  gameState <- liftIO $ getCurrentGameState gameRef
  return $ gameStateToHtml gameState

-- Advance game and return new state as HTML row
-- TODO:
--  - Move this to Game module
--  - Refactor logic so that strike action is not determined by even/odd dice roll.
--    It should instead use the batting average, like it does if a strike action is rolled.
advanceGameDataFrame :: GameRef -> Handler Html
advanceGameDataFrame gameRef = do
  currentState <- liftIO $ getCurrentGameState gameRef
  if isGameOver currentState
    then return $ gameStateToHtml currentState
    else do
      (_, newState) <- liftIO $ advanceGameState gameRef
      return $ gameStateToHtml newState

-- Convert GameState to game container (button + frame) for HTMX updates
gameStateToHtml :: GameState -> Html
gameStateToHtml gs = gameContainerHtml gs

-- Game container with button and frame
gameContainerHtml :: GameState -> Html
gameContainerHtml gs = H.div ! A.id (stringValue "game-container") $ do
  if not (isGameOver gs)
    then H.button
      ! Htmx.hxGet (stringValue "/data")
      ! Htmx.hxTarget (stringValue "#game-container")
      ! Htmx.hxSwap (stringValue "outerHTML")
      ! A.style (stringValue "display: block; margin: 0 auto 20px auto; padding: 10px 20px; background: #3498db; color: white; border: none; border-radius: 5px; cursor: pointer;")
      $ H.toHtml "Continue Game"
    else H.div ! A.style (stringValue "text-align: center; margin: 20px;") $ do
      H.p ! A.style (stringValue "font-size: 1.2em; color: #2c3e50;") $ H.toHtml "Game Complete!"
      H.button
        ! A.onclick (stringValue "window.location.href='/'")
        ! A.style (stringValue "padding: 10px 20px; background: #27ae60; color: white; border: none; border-radius: 5px; cursor: pointer; margin: 10px;")
        $ H.toHtml "Start New Game"
  gameFrameHtml gs

-- Complete game page with header, button, and styling
completeGamePageHtml :: GameState -> Html
completeGamePageHtml gs = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset (stringValue "UTF-8")
    H.meta ! A.name (stringValue "viewport") ! A.content (stringValue "width=device-width, initial-scale=1.0")
    H.title $ H.toHtml "Baseball Game"
    H.script ! A.src (stringValue "https://unpkg.com/htmx.org@1.5.0") $ H.toHtml ""
    H.style $ H.toHtml gamePageCSS
  H.body $ do
    H.h1 ! A.style (stringValue "text-align: center; color: #2c3e50;") $ H.toHtml "Baseball Game"
    gameContainerHtml gs

-- Game frame HTML (the actual game content)
gameFrameHtml :: GameState -> Html
gameFrameHtml gs = H.div ! A.id (stringValue "game-frame") ! A.class_ (stringValue "game-frame") $ do
  -- Main container with side-by-side layout
  H.div ! A.class_ (stringValue "main-game-container") ! A.style (stringValue "display: flex; gap: 20px;") $ do
    -- Left side: Game content
    H.div ! A.class_ (stringValue "game-content") ! A.style (stringValue "flex: 2;") $ do
      -- Scoreboard
      H.div ! A.class_ (stringValue "scoreboard") $ do
        H.div ! A.class_ (stringValue "score-section") $ do
          H.h3 (H.toHtml "Away")
          H.div ! A.class_ (stringValue "score") $ H.toHtml $ show $ awayScore gs
        H.div ! A.class_ (stringValue "score-section") $ do
          H.h3 (H.toHtml "Home")
          H.div ! A.class_ (stringValue "score") $ H.toHtml $ show $ homeScore gs

      -- Game Info
      H.div ! A.class_ (stringValue "game-info") $ do
        if isGameOver gs
          then do
            H.h2 ! A.style (stringValue "color: #e74c3c; font-size: 2.5em; text-align: center; margin: 20px 0;") $ H.toHtml "GAME OVER"
            H.h3 ! A.style (stringValue "text-align: center; color: #2c3e50;") $
              H.toHtml $
                let winner = if homeScore gs > awayScore gs then "Home" else "Away"
                    finalScore = show (awayScore gs) ++ "-" ++ show (homeScore gs)
                 in winner ++ " Team Wins! Final Score: " ++ finalScore
          else H.h2 $ H.toHtml $ "Inning " ++ show (inning gs) ++ " - " ++ show (halfInning gs)

      -- Baseball Diamond
      H.div ! A.class_ (stringValue "diamond-container") $ do
        H.div ! A.class_ (stringValue "diamond") $ H.toHtml ""
        -- Home plate
        H.div ! A.class_ (stringValue (if isJust (home $ bases gs) then "base home-plate occupied" else "base home-plate")) $ H.toHtml ""
        -- First base
        H.div ! A.class_ (stringValue (if isJust (first $ bases gs) then "base first-base occupied" else "base first-base")) $ H.toHtml ""
        -- Second base
        H.div ! A.class_ (stringValue (if isJust (second $ bases gs) then "base second-base occupied" else "base second-base")) $ H.toHtml ""
        -- Third base
        H.div ! A.class_ (stringValue (if isJust (third $ bases gs) then "base third-base occupied" else "base third-base")) $ H.toHtml ""

      -- Current Batter Info
      H.div ! A.class_ (stringValue "batter-info") $ do
        H.h3 (H.toHtml "Current Batter")
        H.p $ H.toHtml $ maybe "None" (\player -> Game.Logic.name player ++ " (#" ++ show (Game.Logic.number player) ++ ")") $ currentBatter gs

      -- Count Display
      H.div ! A.class_ (stringValue "count") $ do
        H.div ! A.class_ (stringValue "count-item") $ do
          H.div ! A.class_ (stringValue "number") $ H.toHtml $ show $ balls gs
          H.div ! A.class_ (stringValue "label") $ H.toHtml "BALLS"
        H.div ! A.class_ (stringValue "count-item") $ do
          H.div ! A.class_ (stringValue "number") $ H.toHtml $ show $ strikes gs
          H.div ! A.class_ (stringValue "label") $ H.toHtml "STRIKES"
        H.div ! A.class_ (stringValue "count-item") $ do
          H.div ! A.class_ (stringValue "number") $ H.toHtml $ show $ outs gs
          H.div ! A.class_ (stringValue "label") $ H.toHtml "OUTS"

    -- Right side: Game Log
    H.div ! A.class_ (stringValue "game-log") ! A.style (stringValue "flex: 1; min-width: 300px;") $ do
      H.h3 ! A.style (stringValue "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;") $ H.toHtml "Game Log"
      if null (pitchLog gs)
        then H.p ! A.style (stringValue "color: #7f8c8d; font-style: italic;") $ H.toHtml "No pitches yet..."
        else
          H.div ! A.class_ (stringValue "log-entries") ! A.style (stringValue "max-height: 500px; overflow-y: auto; border: 1px solid #bdc3c7; border-radius: 5px;") $
            mapM_ renderLogEntry (pitchLog gs)

-- Render individual log entry
renderLogEntry :: Log -> Html
renderLogEntry logEntry =
  H.div ! A.class_ (stringValue "log-entry") ! A.style (stringValue "padding: 10px; border-bottom: 1px solid #ecf0f1; background: #f8f9fa;") $ do
    H.div ! A.style (stringValue "display: flex; justify-content: space-between; align-items: center;") $ do
      H.div ! A.style (stringValue "font-weight: bold; color: #2c3e50;") $ do
        H.toHtml $ maybe "Unknown Batter" (\player -> Game.Logic.name player ++ " (#" ++ show (Game.Logic.number player) ++ ")") (currentBatter_ logEntry)
        H.span ! A.style (stringValue "margin-left: 10px; font-weight: normal; color: #7f8c8d;") $
          H.toHtml $
            "Inning " ++ show (inning_ logEntry) ++ " - " ++ show (halfInning_ logEntry)
      H.div ! A.style (stringValue "font-weight: bold;") $ do
        case strikeAction_ logEntry of
          NoAction -> H.span ! A.style (stringValue "color: #95a5a6;") $ H.toHtml "Ball"
          CalledStrike -> H.span ! A.style (stringValue "color: #e74c3c;") $ H.toHtml "Strike"
          HitSingle -> H.span ! A.style (stringValue "color: #27ae60;") $ H.toHtml "Single"
          HitDouble -> H.span ! A.style (stringValue "color: #27ae60;") $ H.toHtml "Double"
          HitTriple -> H.span ! A.style (stringValue "color: #27ae60;") $ H.toHtml "Triple"
          HomeRun -> H.span ! A.style (stringValue "color: #f39c12;") $ H.toHtml "Home Run!"
          GroundOut -> H.span ! A.style (stringValue "color: #e74c3c;") $ H.toHtml "Ground Out"
          FlyOut -> H.span ! A.style (stringValue "color: #e74c3c;") $ H.toHtml "Fly Out"
          PopOut -> H.span ! A.style (stringValue "color: #e74c3c;") $ H.toHtml "Pop Out"
          FieldingError -> H.span ! A.style (stringValue "color: #f39c12;") $ H.toHtml "Fielding Error"
          HitByPitch -> H.span ! A.style (stringValue "color: #9b59b6;") $ H.toHtml "Hit By Pitch"
    H.div ! A.style (stringValue "margin-top: 5px; font-size: 0.9em; color: #7f8c8d;") $ do
      H.toHtml $
        "Score: "
          ++ show (awayScore_ logEntry)
          ++ "-"
          ++ show (homeScore_ logEntry)
          ++ " | Count: "
          ++ show (balls_ logEntry)
          ++ "-"
          ++ show (strikes_ logEntry)
          ++ " | Outs: "
          ++ show (outs_ logEntry)

-- Configuration page handler
configPageHandler :: GameRef -> Handler Html
configPageHandler gameRef = do
  gameState <- liftIO $ getCurrentGameState gameRef
  return $ configPageToHtml (homeBatting gameState) (awayBatting gameState)

-- Start game handler - returns complete game page
startGameHandler :: GameRef -> Handler Html
startGameHandler gameRef = do
  gameState <- liftIO $ getCurrentGameState gameRef
  return $ completeGamePageHtml gameState

-- Update player handler
updatePlayerHandler :: GameRef -> [(String, String)] -> Handler Html
updatePlayerHandler gameRef formData = do
  gameState <- liftIO $ getCurrentGameState gameRef
  let updatedGameState = updatePlayerFromForm gameState formData
  liftIO $ writeIORef gameRef updatedGameState
  return $ configPageToHtml (homeBatting updatedGameState) (awayBatting updatedGameState)

-- Helper function to update player from form data
updatePlayerFromForm :: GameState -> [(String, String)] -> GameState
updatePlayerFromForm gameState formData =
  let getFormValue key = lookup key formData
      teamType = getFormValue "team"
      playerIndex = getFormValue "player" >>= readMaybe
      newName = getFormValue "name"
      newNumber = getFormValue "number" >>= readMaybe
      newBattingAvg = getFormValue "battingAverage" >>= readMaybe
      newSlugging = getFormValue "sluggingPercentage" >>= readMaybe
   in case (teamType, playerIndex) of
        (Just "home", Just idx) ->
          let updatedHomeTeam = updatePlayerAtIndex (homeBatting gameState) idx newName newNumber newBattingAvg newSlugging
           in gameState {homeBatting = updatedHomeTeam}
        (Just "away", Just idx) ->
          let updatedAwayTeam = updatePlayerAtIndex (awayBatting gameState) idx newName newNumber newBattingAvg newSlugging
           in gameState {awayBatting = updatedAwayTeam}
        _ -> gameState

-- Helper to update a player at a specific index
updatePlayerAtIndex :: [Player] -> Int -> Maybe String -> Maybe Int -> Maybe Double -> Maybe Double -> [Player]
updatePlayerAtIndex players idx mName mNumber mBattingAvg mSlugging =
  let updatePlayer player =
        player
          { name = maybe (Game.Logic.name player) Prelude.id mName,
            number = maybe (Game.Logic.number player) Prelude.id mNumber,
            battingAverage = maybe (battingAverage player) (Prelude.max 0.150 . Prelude.min 0.400) mBattingAvg,
            sluggingPercentage = maybe (sluggingPercentage player) (Prelude.max 0.300 . Prelude.min 0.700) mSlugging
          }
   in case List.splitAt idx players of
        (before, p : after) -> before ++ [updatePlayer p] ++ after
        _ -> players

-- Generate configuration page HTML
configPageToHtml :: [Player] -> [Player] -> Html
configPageToHtml homeTeamPlayers awayTeamPlayers = do
  H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toHtml "Baseball Dice Game - Team Configuration"
      H.script ! A.src (stringValue "https://unpkg.com/htmx.org@1.9.10") $ H.toHtml ""
      H.style $ H.toHtml configPageCSS
    H.body $ do
      H.div ! A.class_ (stringValue "config-container") $ do
        H.h1 $ H.toHtml "Team Configuration"
        H.div ! A.class_ (stringValue "teams-container") $ do
          H.div ! A.class_ (stringValue "team-section") $ do
            H.h2 $ H.toHtml "Home Team"
            mapM_ (renderPlayerForm "home") (zip [0..] homeTeamPlayers)
          H.div ! A.class_ (stringValue "team-section") $ do
            H.h2 $ H.toHtml "Away Team"
            mapM_ (renderPlayerForm "away") (zip [0..] awayTeamPlayers)
        H.div ! A.class_ (stringValue "start-button-container") $ do
          H.form ! A.action (stringValue "/start-game") ! A.method (stringValue "post") $
            H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "start-game-btn") $ H.toHtml "Start Game"

-- Render individual player configuration form
renderPlayerForm :: String -> (Int, Player) -> Html
renderPlayerForm teamType (idx, player) = do
  H.div ! A.class_ (stringValue "player-form") ! A.id (stringValue $ "player-" ++ teamType ++ "-" ++ show idx) $ do
    H.h3 $ H.toHtml $ Game.Logic.name player ++ " (#" ++ show (Game.Logic.number player) ++ ")"
    H.form ! Htmx.hxPost (stringValue "/update-player")
           ! Htmx.hxTarget (stringValue $ "#player-" ++ teamType ++ "-" ++ show idx)
           ! Htmx.hxSwap (stringValue "outerHTML") $ do
      H.input ! A.type_ (stringValue "hidden") ! A.name (stringValue "team") ! A.value (stringValue teamType)
      H.input ! A.type_ (stringValue "hidden") ! A.name (stringValue "player") ! A.value (stringValue $ show idx)
      
      H.div ! A.class_ (stringValue "form-row") $ do
        H.label $ H.toHtml "Name: "
        H.input ! A.type_ (stringValue "text") ! A.name (stringValue "name") ! A.value (stringValue $ Game.Logic.name player)
      
      H.div ! A.class_ (stringValue "form-row") $ do
        H.label $ H.toHtml "Number: "
        H.input ! A.type_ (stringValue "number") ! A.name (stringValue "number") ! A.value (stringValue $ show $ Game.Logic.number player) ! A.min (stringValue "1") ! A.max (stringValue "99")
      
      H.div ! A.class_ (stringValue "form-row") $ do
        H.label $ H.toHtml "Batting Average: "
        H.input ! A.type_ (stringValue "number") ! A.name (stringValue "battingAverage") ! A.value (stringValue $ show $ battingAverage player) 
                ! A.min (stringValue "0.150") ! A.max (stringValue "0.400") ! A.step (stringValue "0.001")
      
      H.div ! A.class_ (stringValue "form-row") $ do
        H.label $ H.toHtml "Slugging Percentage: "
        H.input ! A.type_ (stringValue "number") ! A.name (stringValue "sluggingPercentage") ! A.value (stringValue $ show $ sluggingPercentage player)
                ! A.min (stringValue "0.300") ! A.max (stringValue "0.700") ! A.step (stringValue "0.001")
      
      H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "update-btn") $ H.toHtml "Update"

-- Game redirect HTML (redirects to main game)
gameRedirectHtml :: Html
gameRedirectHtml = do
  H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toHtml "Starting Game..."
      H.meta ! A.httpEquiv (stringValue "refresh") ! A.content (stringValue "0;url=/data")
    H.body $ do
      H.p $ H.toHtml "Starting game..."

-- CSS for configuration page
configPageCSS :: String
configPageCSS = unlines
  [ "body { font-family: Arial, sans-serif; margin: 0; padding: 20px; background: #f5f5f5; }",
    ".config-container { max-width: 1200px; margin: 0 auto; }",
    "h1 { text-align: center; color: #2c3e50; margin-bottom: 40px; }",
    ".teams-container { display: flex; gap: 40px; }",
    ".team-section { flex: 1; background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }",
    "h2 { color: #3498db; border-bottom: 2px solid #3498db; padding-bottom: 10px; }",
    ".player-form { background: #f8f9fa; padding: 15px; margin: 15px 0; border-radius: 5px; border-left: 4px solid #3498db; }",
    "h3 { margin: 0 0 15px 0; color: #2c3e50; }",
    ".form-row { margin: 10px 0; display: flex; align-items: center; }",
    "label { min-width: 150px; font-weight: bold; }",
    "input { padding: 5px; border: 1px solid #ddd; border-radius: 3px; flex: 1; margin-left: 10px; }",
    ".update-btn { background: #27ae60; color: white; border: none; padding: 5px 15px; border-radius: 3px; cursor: pointer; margin-top: 10px; }",
    ".update-btn:hover { background: #229954; }",
    ".start-button-container { text-align: center; margin: 40px 0; }",
    ".start-game-btn { background: #e74c3c; color: white; border: none; padding: 15px 40px; font-size: 18px; border-radius: 5px; cursor: pointer; }",
    ".start-game-btn:hover { background: #c0392b; }"
  ]

-- CSS styles for the game page (based on original generateHTMXPage)
gamePageCSS :: String
gamePageCSS = unlines
  [ ".game-frame { font-family: Arial, sans-serif; max-width: 800px; margin: 20px auto; }",
    ".scoreboard { display: flex; justify-content: space-between; background: #2c3e50; color: white; padding: 15px; border-radius: 8px; margin-bottom: 20px; }",
    ".score-section h3 { margin: 0 0 5px 0; }",
    ".score-section .score { font-size: 2em; font-weight: bold; }",
    ".game-info { text-align: center; margin-bottom: 20px; }",
    ".diamond-container { position: relative; width: 300px; height: 300px; margin: 0 auto; }",
    ".diamond { width: 200px; height: 200px; background: #8B4513; transform: rotate(45deg); position: absolute; top: 50px; left: 50px; border-radius: 15px; }",
    ".base { position: absolute; width: 20px; height: 20px; background: white; border: 2px solid #333; }",
    ".base.occupied { background: #ff6b35; }",
    ".first-base { top: 140px; right: 40px; }",
    ".second-base { top: 40px; right: 140px; }",
    ".third-base { top: 140px; left: 40px; }",
    ".home-plate { bottom: 40px; left: 140px; border-radius: 50%; }",
    ".batter-info { text-align: center; margin: 20px 0; padding: 15px; background: #ecf0f1; border-radius: 8px; }",
    ".count { display: flex; justify-content: center; gap: 30px; margin-top: 20px; }",
    ".count-item { text-align: center; }",
    ".count-item .number { font-size: 2em; font-weight: bold; color: #2c3e50; }",
    ".count-item .label { font-size: 0.9em; color: #7f8c8d; }",
    ".game-log { background: white; border-radius: 8px; padding: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }",
    ".log-entries { scrollbar-width: thin; scrollbar-color: #bdc3c7 #f8f9fa; }",
    ".log-entries::-webkit-scrollbar { width: 8px; }",
    ".log-entries::-webkit-scrollbar-track { background: #f8f9fa; }",
    ".log-entries::-webkit-scrollbar-thumb { background: #bdc3c7; border-radius: 4px; }"
  ]
