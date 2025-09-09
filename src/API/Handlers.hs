module API.Handlers where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (writeIORef)
import Game.Logic (GameState (..), isGameOver, Player (..))
import Game.Season (SeasonRef, SeasonState (..), runStartNextGame, runRecordGameResult, getCurrentSeasonState, newSeasonState, runAdvanceCurrentGame)
import Servant
import Text.Blaze.Html5 as H
import Text.Read (readMaybe)
import View.HTMX (seasonPageToHtml, seasonConfigPageToHtml, autoAdvancingGamePageHtml, autoAdvancingGameFrameHtml, updatePlayerAtIndex)

-- Season page handler - main landing page
seasonPageHandler :: SeasonRef -> Handler Html
seasonPageHandler seasonRef = do
  seasonState <- liftIO $ getCurrentSeasonState seasonRef
  return $ seasonPageToHtml seasonState

-- Start new season handler
startNewSeasonHandler :: SeasonRef -> Handler Html
startNewSeasonHandler seasonRef = do
  -- Create default teams (reusing logic from Game.State)
  let homeTeam =
        [ Player "Home A" 1 0.285 0.350 0.450,
          Player "Home B" 2 0.312 0.380 0.520,
          Player "Home C" 3 0.267 0.330 0.425,
          Player "Home D" 4 0.298 0.375 0.580,
          Player "Home E" 5 0.245 0.315 0.390,
          Player "Home F" 6 0.278 0.340 0.465,
          Player "Home G" 7 0.292 0.360 0.485,
          Player "Home H" 8 0.255 0.325 0.410,
          Player "Home I" 9 0.220 0.280 0.340
        ]
  let awayTeam =
        [ Player "Away A" 1 0.275 0.345 0.440,
          Player "Away B" 2 0.305 0.370 0.510,
          Player "Away C" 3 0.258 0.320 0.415,
          Player "Away D" 4 0.289 0.365 0.565,
          Player "Away E" 5 0.235 0.305 0.380,
          Player "Away F" 6 0.270 0.335 0.455,
          Player "Away G" 7 0.284 0.355 0.475,
          Player "Away H" 8 0.248 0.318 0.400,
          Player "Away I" 9 0.210 0.270 0.320
        ]
  
  let newSeason = newSeasonState homeTeam awayTeam
  liftIO $ writeIORef seasonRef newSeason
  return $ seasonConfigPageToHtml homeTeam awayTeam

-- Season configuration page handler
seasonConfigPageHandler :: SeasonRef -> Handler Html
seasonConfigPageHandler seasonRef = do
  seasonState <- liftIO $ getCurrentSeasonState seasonRef
  return $ seasonConfigPageToHtml (homeTeam seasonState) (awayTeam seasonState)

-- Start current season game handler
startSeasonGameHandler :: SeasonRef -> Handler Html
startSeasonGameHandler seasonRef = do
  maybeGameState <- liftIO $ runStartNextGame seasonRef
  case maybeGameState of
    Nothing -> do
      -- Season is complete, redirect to season page
      seasonState <- liftIO $ getCurrentSeasonState seasonRef
      return $ seasonPageToHtml seasonState
    Just gameState -> do
      -- Start auto-advancing game
      return $ autoAdvancingGamePageHtml gameState

-- Auto-advance season game data frame
-- Uses the persistent game state tracking in Season module
advanceSeasonGameDataFrame :: SeasonRef -> Handler Html
advanceSeasonGameDataFrame seasonRef = do
  -- Advance the current game by one step, maintaining all game state including pitch log
  maybeGameState <- liftIO $ runAdvanceCurrentGame seasonRef
  case maybeGameState of
    Nothing -> do
      -- No current game, return season page
      seasonState <- liftIO $ getCurrentSeasonState seasonRef
      return $ seasonPageToHtml seasonState
    Just gameState -> do
      if isGameOver gameState
        then do
          -- Game finished, record result and return season page
          liftIO $ runRecordGameResult seasonRef gameState
          updatedSeasonState <- liftIO $ getCurrentSeasonState seasonRef
          return $ seasonPageToHtml updatedSeasonState
        else do
          -- Game still ongoing, return game frame with preserved state
          return $ autoAdvancingGameFrameHtml gameState

-- Finish season game handler
finishSeasonGameHandler :: SeasonRef -> Handler Html
finishSeasonGameHandler seasonRef = do
  seasonState <- liftIO $ getCurrentSeasonState seasonRef
  return $ seasonPageToHtml seasonState

-- Next season game handler
nextSeasonGameHandler :: SeasonRef -> Handler Html
nextSeasonGameHandler seasonRef = do
  seasonState <- liftIO $ getCurrentSeasonState seasonRef
  return $ seasonConfigPageToHtml (homeTeam seasonState) (awayTeam seasonState)

-- Update season player handler
updateSeasonPlayerHandler :: SeasonRef -> [(String, String)] -> Handler Html
updateSeasonPlayerHandler seasonRef formData = do
  seasonState <- liftIO $ getCurrentSeasonState seasonRef
  let updatedSeasonState = updateSeasonPlayerFromForm seasonState formData
  liftIO $ writeIORef seasonRef updatedSeasonState
  return $ seasonConfigPageToHtml (homeTeam updatedSeasonState) (awayTeam updatedSeasonState)

-- Helper function to update season player from form data
updateSeasonPlayerFromForm :: SeasonState -> [(String, String)] -> SeasonState
updateSeasonPlayerFromForm seasonState formData =
  let getFormValue key = lookup key formData
      teamType = getFormValue "team"
      playerIndex = getFormValue "player" >>= readMaybe
      newName = getFormValue "name"
      newNumber = getFormValue "number" >>= readMaybe
      newBattingAvg = getFormValue "battingAverage" >>= readMaybe
      newSlugging = getFormValue "sluggingPercentage" >>= readMaybe
   in case (teamType, playerIndex) of
        (Just "home", Just idx) ->
          let updatedHomeTeam = updatePlayerAtIndex (homeTeam seasonState) idx newName newNumber newBattingAvg newSlugging
           in seasonState {homeTeam = updatedHomeTeam}
        (Just "away", Just idx) ->
          let updatedAwayTeam = updatePlayerAtIndex (awayTeam seasonState) idx newName newNumber newBattingAvg newSlugging
           in seasonState {awayTeam = updatedAwayTeam}
        _ -> seasonState
