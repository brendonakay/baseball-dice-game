module API.Handlers where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Game.Logic (BasesState (..), GameState (..), HalfInning (..), Log (..), Player (..), StrikeAction (..))
import Game.State (GameRef, advanceGameState, getCurrentGameState)
import Servant
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (stringValue)

-- Get current game state and render as HTML row
getGameDataRow :: GameRef -> Handler Html
getGameDataRow gameRef = do
  gameState <- liftIO $ getCurrentGameState gameRef
  return $ gameStateToHtml gameState

-- Advance game and return new state as HTML row
advanceGameDataRow :: GameRef -> Handler Html
advanceGameDataRow gameRef = do
  (_, newState) <- liftIO $ advanceGameState gameRef
  return $ gameStateToHtml newState

-- Convert GameState to HTML Frame with baseball diamond
gameStateToHtml :: GameState -> Html
gameStateToHtml gs = H.div ! A.id (stringValue "game-frame") ! A.class_ (stringValue "game-frame") $ do
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
        H.h2 $ H.toHtml $ "Inning " ++ show (inning gs) ++ " - " ++ show (halfInning gs)

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
            mapM_ renderLogEntry (reverse $ pitchLog gs)

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
