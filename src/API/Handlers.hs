module API.Handlers where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Game.Logic (BasesState (..), GameState (..), HalfInning (..), Player (..))
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
