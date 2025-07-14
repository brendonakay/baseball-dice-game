module API.Handlers where

import Control.Monad.IO.Class (liftIO)
import Game.Logic (GameState (..), HalfInning (..), Player (..))
import Game.State (GameRef, advanceGameState, getCurrentGameState)
import Servant
import Text.Blaze.Html5 as H

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

-- Convert GameState to HTML table row
gameStateToHtml :: GameState -> Html
gameStateToHtml gs = H.tr $ do
  H.td $ H.toHtml $ show $ homeScore gs
  H.td $ H.toHtml $ show $ awayScore gs
  H.td $ H.toHtml $ show $ inning gs
  H.td $ H.toHtml $ show $ halfInning gs
  H.td $ H.toHtml $ maybe "None" (\player -> name player ++ " (#" ++ show (number player) ++ ")") $ currentBatter gs
  H.td $ H.toHtml $ show $ balls gs
  H.td $ H.toHtml $ show $ strikes gs
  H.td $ H.toHtml $ show $ outs gs
