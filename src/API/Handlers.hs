{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Handlers where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IORef (writeIORef)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection)
import Servant
import Servant.Auth.Server as SAS
import Text.Blaze.Html5 as H
import Text.Read (readMaybe)
import User.Auth (LoginCredentials (..), RegisterData (..), authenticateUser, createUser, fetchUserCards, loadSeasonTeams, validateRegistration)
import User.AuthenticatedUser (AuthenticatedUser (..))
import View.Auth (loginErrorHtml, loginFailedHtml, loginPageHtml, loginSuccessRedirectHtml, logoutHtml, registrationErrorHtml, registrationFailedHtml, registrationSuccessHtml)
import View.Config (updatePlayerAtIndex)
import View.Game (activeGameFragment, autoAdvancingGameFrameHtml, gameCompletionHtml)
import View.Games (gamesPageToHtml)
import View.PersonalCollection (personalCollectionPageToHtml)
import View.Season (gameFrameSeasonFragment, renderSeasonPlayerForm, seasonConfigPageToHtml, seasonPageToHtml)
import View.User (userPageToHtml)
import WaxBall.Game (Player (..), isGameOver)
import WaxBall.Season (GameResult (..), SeasonRef, SeasonState (..), getCurrentSeasonState, newSeasonState, runAdvanceCurrentGame, runRecordGameResult, runStartNextGame)

loginPageHandler :: Handler Html
loginPageHandler = return loginPageHtml

-- Login handler - processes login form
loginHandler :: Connection -> SAS.CookieSettings -> SAS.JWTSettings -> [(String, String)] -> Handler (Headers '[Header "Set-Cookie" SAS.SetCookie, Header "HX-Redirect" String] Html)
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
          maybeSessionCookie <- liftIO $ SAS.makeSessionCookie cookieSettings jwtSettings user
          case maybeSessionCookie of
            Just sCookie ->
              -- HTMX reads the HX-Redirect response header and navigates client-side,
              -- so no inline <script> redirect is needed.
              return $ addHeader sCookie $ addHeader "/user" loginSuccessRedirectHtml
            Nothing -> throwError err500 {errBody = L8.pack "Failed to create authentication cookie"}
        Nothing -> return $ noHeader $ noHeader loginFailedHtml
    _ -> return $ noHeader $ noHeader loginErrorHtml

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

-- User page handler - user dashboard with game shell
userPageHandler :: AuthenticatedUser -> SeasonRef -> Handler Html
userPageHandler user seasonRef = do
  seasonState <- liftIO $ getCurrentSeasonState seasonRef
  return $ userPageToHtml user seasonState

-- Personal collection page handler - displays user's card collection
personalCollectionPageHandler :: Connection -> AuthenticatedUser -> Handler Html
personalCollectionPageHandler dbConn user = do
  userCards <- liftIO $ fetchUserCards dbConn (auId user)
  let userWithCards = user {personalCollection = userCards}
  return $ personalCollectionPageToHtml userWithCards

-- Season page handler
seasonPageHandler :: AuthenticatedUser -> SeasonRef -> Handler Html
seasonPageHandler user seasonRef = do
  seasonState <- liftIO $ getCurrentSeasonState seasonRef
  return $ seasonPageToHtml user seasonState

-- Games stub page handler
gamesPageHandler :: AuthenticatedUser -> Handler Html
gamesPageHandler user = return $ gamesPageToHtml user

-- Game frame handler - returns HTMX fragment for the landing page game shell
-- If a game is active, returns the auto-advancing game container.
-- Otherwise, returns the season summary fragment.
gameFrameHandler :: AuthenticatedUser -> SeasonRef -> Handler Html
gameFrameHandler _user seasonRef = do
  seasonState <- liftIO $ getCurrentSeasonState seasonRef
  case currentGameState seasonState of
    Just gs -> return $ activeGameFragment gs
    Nothing -> return $ gameFrameSeasonFragment seasonState

-- Start new season handler
startNewSeasonHandler :: Connection -> AuthenticatedUser -> SeasonRef -> Handler Html
startNewSeasonHandler dbConn user seasonRef = do
  (homeTeam, awayTeam, homePitcher, awayPitcher) <- liftIO $ loadSeasonTeams dbConn
  let newSeason = newSeasonState homeTeam awayTeam homePitcher awayPitcher
  liftIO $ writeIORef seasonRef newSeason
  return $ seasonConfigPageToHtml user homeTeam awayTeam

-- Season configuration page handler
seasonConfigPageHandler :: AuthenticatedUser -> SeasonRef -> Handler Html
seasonConfigPageHandler user seasonRef = do
  seasonState <- liftIO $ getCurrentSeasonState seasonRef
  return $ seasonConfigPageToHtml user (homeTeam seasonState) (awayTeam seasonState)

-- Start current season game handler — Post-Redirect-Get to /user
-- The game shell on /user will detect the active game via /game-frame
startSeasonGameHandler :: SeasonRef -> Handler Html
startSeasonGameHandler seasonRef = do
  _ <- liftIO $ runStartNextGame seasonRef
  throwError err303 {errHeaders = [("Location", "/user")]}

-- Auto-advance season game data frame
advanceSeasonGameDataFrame :: AuthenticatedUser -> SeasonRef -> Handler Html
advanceSeasonGameDataFrame user seasonRef = do
  maybeGameState <- liftIO $ runAdvanceCurrentGame seasonRef
  case maybeGameState of
    Nothing -> do
      seasonState <- liftIO $ getCurrentSeasonState seasonRef
      case gameResults seasonState of
        [] -> return $ userPageToHtml user seasonState
        (mostRecent : _) -> return $ gameCompletionHtml (gameState mostRecent)
    Just gs -> do
      if isGameOver gs
        then do
          liftIO $ runRecordGameResult seasonRef gs
          return $ gameCompletionHtml gs
        else return $ autoAdvancingGameFrameHtml gs

-- Next season game handler — returns season config page
nextSeasonGameHandler :: AuthenticatedUser -> SeasonRef -> Handler Html
nextSeasonGameHandler user seasonRef = do
  seasonState <- liftIO $ getCurrentSeasonState seasonRef
  return $ seasonConfigPageToHtml user (homeTeam seasonState) (awayTeam seasonState)

-- Update season player handler — returns the updated player form fragment
updateSeasonPlayerHandler :: AuthenticatedUser -> SeasonRef -> [(String, String)] -> Handler (Headers '[Header "HX-Trigger" String] Html)
updateSeasonPlayerHandler _user seasonRef formData = do
  seasonState <- liftIO $ getCurrentSeasonState seasonRef
  let updatedSeasonState = updateSeasonPlayerFromForm seasonState formData
  liftIO $ writeIORef seasonRef updatedSeasonState
  let teamType = fromMaybe "home" $ lookup "team" formData
      idx = fromMaybe 0 $ lookup "player" formData >>= readMaybe
      players =
        if teamType == "home"
          then homeTeam updatedSeasonState
          else awayTeam updatedSeasonState
      updatedPlayer = players !! idx
  -- HX-Trigger fires a client-side `toast` event; the TypeScript toast island
  -- renders the notification. Server decides when, client decides how.
  return $
    addHeader "{\"toast\":{\"message\":\"Player updated\"}}" $
      renderSeasonPlayerForm teamType (idx, updatedPlayer)

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
