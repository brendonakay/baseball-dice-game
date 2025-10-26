{-# LANGUAGE DataKinds #-}

module API.Handlers where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IORef (writeIORef)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection)
import Servant
import Servant.Auth.Server as SAS
import Text.Blaze.Html5 as H
import Text.Read (readMaybe)
import User.Auth (LoginCredentials (..), RegisterData (..), authenticateUser, createUser, validateRegistration)
import User.AuthenticatedUser (AuthenticatedUser (..))
import View.Auth (loginErrorHtml, loginFailedHtml, loginPageHtml, loginSuccessRedirectHtml, logoutHtml, registrationErrorHtml, registrationFailedHtml, registrationSuccessHtml)
import View.Config (updatePlayerAtIndex)
import View.Game (autoAdvancingGameFrameHtml, autoAdvancingGamePageHtml, gameCompletionHtml)
import View.PersonalCollection (personalCollectionPageToHtml)
import View.Season (seasonConfigPageToHtml, seasonPageToHtml)
import View.User (userPageToHtml)
import WaxBall.Game (Player (..), isGameOver)
import WaxBall.Season (GameResult (..), SeasonRef, SeasonState (..), getCurrentSeasonState, newSeasonState, runAdvanceCurrentGame, runRecordGameResult, runStartNextGame)

loginPageHandler :: Handler Html
loginPageHandler = return loginPageHtml

-- Login handler - processes login form
loginHandler :: Connection -> SAS.CookieSettings -> SAS.JWTSettings -> [(String, String)] -> Handler (Headers '[Header "Set-Cookie" SAS.SetCookie] Html)
loginHandler dbConn cookieSettings jwtSettings formData = do
  let getFormValue key = T.pack <$> lookup key formData
      username = getFormValue "username"
      password = getFormValue "password"
  case (username, password) of
    (Just u, Just p) -> do
      let creds = LoginCredentials u p
      maybeUser <- liftIO $ authenticateUser dbConn creds
      case maybeUser of
        Just user -> do
          -- TODO: Move this paragraph to its own function
          -- Create authentication cookie using servant-auth-server
          maybeSessionCookie <- liftIO $ SAS.makeSessionCookie cookieSettings jwtSettings user
          case maybeSessionCookie of
            Just sCookie -> do
              return $ addHeader sCookie loginSuccessRedirectHtml
            Nothing -> throwError err500 {errBody = L8.pack "Failed to create authentication cookie"}
        Nothing -> return $ noHeader loginFailedHtml
    _ -> return $ noHeader loginErrorHtml

-- Register handler - processes registration form
registerHandler :: Connection -> [(String, String)] -> Handler Html
registerHandler dbConn formData = do
  let getFormValue key = T.pack <$> lookup key formData
      username = getFormValue "username"
      email = getFormValue "email"
      password = getFormValue "password"
  case (username, email, password) of
    (Just u, Just e, Just p) -> do
      let regData = RegisterData u e p
      validation <- liftIO $ validateRegistration dbConn regData
      case validation of
        Left errorMsg -> return $ registrationFailedHtml errorMsg
        Right () -> do
          result <- liftIO $ createUser dbConn regData
          case result of
            Left errorMsg -> return $ registrationFailedHtml errorMsg
            Right _ -> return registrationSuccessHtml
    _ -> return registrationErrorHtml

-- Logout handler - clears user session
logoutHandler :: Handler Html
logoutHandler = return logoutHtml

-- User page handler - user dashboard with season info
userPageHandler :: AuthenticatedUser -> SeasonRef -> Handler Html
userPageHandler user seasonRef = do
  seasonState <- liftIO $ getCurrentSeasonState seasonRef
  return $ userPageToHtml user seasonState

-- Authenticated user page handler - for the protected routes
userPageHandlerAuth :: AuthenticatedUser -> SeasonRef -> Handler Html
userPageHandlerAuth user seasonRef = do
  seasonState <- liftIO $ getCurrentSeasonState seasonRef
  return $ userPageToHtml user seasonState

-- Personal collection page handler - displays user's card collection
personalCollectionPageHandler :: AuthenticatedUser -> Handler Html
personalCollectionPageHandler user = do
  return $ personalCollectionPageToHtml user

-- Authenticated personal collection handler
personalCollectionPageHandlerAuth :: AuthenticatedUser -> Handler Html
personalCollectionPageHandlerAuth user = do
  return $ personalCollectionPageToHtml user

-- Start new season handler
startNewSeasonHandler :: SeasonRef -> Handler Html
startNewSeasonHandler seasonRef = do
  -- Create default teams (reusing logic from WaxBall.State)
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
advanceSeasonGameDataFrame :: AuthenticatedUser -> SeasonRef -> Handler Html
advanceSeasonGameDataFrame user seasonRef = do
  -- Advance the current game by one step, maintaining all game state including pitch log
  maybeGameState <- liftIO $ runAdvanceCurrentGame seasonRef
  case maybeGameState of
    Nothing -> do
      -- No current game, show game completion with stats from most recent game
      seasonState <- liftIO $ getCurrentSeasonState seasonRef
      case gameResults seasonState of
        [] -> do
          -- No games completed yet, fallback to user page
          return $ userPageToHtml user seasonState
        (mostRecent : _) -> do
          -- Use the most recent completed game state to show completion screen
          return $ gameCompletionHtml (gameState mostRecent)
    Just gameState -> do
      if isGameOver gameState
        then do
          -- Game finished, record result and return completion view
          liftIO $ runRecordGameResult seasonRef gameState
          return $ gameCompletionHtml gameState
        else do
          -- Game still ongoing, return game frame with preserved state
          return $ autoAdvancingGameFrameHtml gameState

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
