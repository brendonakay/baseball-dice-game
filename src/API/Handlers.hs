module API.Handlers where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (writeIORef)
import Game.Logic (GameState (..), isGameOver)
import Game.State (GameRef, advanceGameState, getCurrentGameState)
import Servant
import Text.Blaze.Html5 as H
import View.HTMX

-- Get current game state and render as HTML row
getGameDataRow :: GameRef -> Handler Html
getGameDataRow gameRef = do
  gameState <- liftIO $ getCurrentGameState gameRef
  return $ gameStateToHtml gameState

-- Advance game and return new state as HTML row
-- TODO:
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
