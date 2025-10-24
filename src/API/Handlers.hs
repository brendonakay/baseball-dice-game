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
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx as Htmx
import Text.Read (readMaybe)
import User.Auth (LoginCredentials (..), RegisterData (..), authenticateUser, createUser, validateRegistration)
import User.AuthenticatedUser (AuthenticatedUser (..))
import View.HTMX (autoAdvancingGameFrameHtml, autoAdvancingGamePageHtml, gameCompletionHtml, seasonConfigPageToHtml, seasonPageToHtml, updatePlayerAtIndex)
import View.PersonalCollection (personalCollectionPageToHtml)
import View.User (userPageToHtml)
import WaxBall.Game (Player (..), isGameOver)
import WaxBall.Season (GameResult (..), SeasonRef, SeasonState (..), getCurrentSeasonState, newSeasonState, runAdvanceCurrentGame, runRecordGameResult, runStartNextGame)

-- TODO:
-- - Move HTML logic to View module

-- Login page handler - shows login/register form
loginPageHandler :: Handler Html
loginPageHandler = do
  return $ H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toHtml "Baseball Dice Game - Login"
      H.meta ! A.charset (stringValue "utf-8")
      H.meta ! A.name (stringValue "viewport") ! A.content (stringValue "width=device-width, initial-scale=1")
      H.script ! A.src (stringValue "https://unpkg.com/htmx.org@1.9.10") $ H.toHtml ""
    H.body ! A.style (stringValue "background: #f5f5f5; font-family: Arial, sans-serif; margin: 0; padding: 0; min-height: 100vh;") $ do
      H.div ! A.style (stringValue "max-width: 400px; margin: 50px auto; padding: 20px; background: white; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);") $ do
        H.h1 ! A.style (stringValue "text-align: center; color: #2c3e50; margin-bottom: 30px;") $ H.toHtml "Baseball Dice Game"

        H.h2 ! A.style (stringValue "color: #3498db; border-bottom: 2px solid #3498db; padding-bottom: 10px;") $ H.toHtml "Login"
        H.form ! Htmx.hxPost (stringValue "/login") ! Htmx.hxTarget (stringValue "body") $ do
          H.div ! A.style (stringValue "margin-bottom: 15px;") $ do
            H.label ! A.for (stringValue "username") ! A.style (stringValue "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Username:"
            H.input ! A.type_ (stringValue "text") ! A.name (stringValue "username") ! A.id (stringValue "username") ! A.required (stringValue "") ! A.style (stringValue "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
          H.div ! A.style (stringValue "margin-bottom: 15px;") $ do
            H.label ! A.for (stringValue "password") ! A.style (stringValue "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Password:"
            H.input ! A.type_ (stringValue "password") ! A.name (stringValue "password") ! A.id (stringValue "password") ! A.required (stringValue "") ! A.style (stringValue "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
          H.button ! A.type_ (stringValue "submit") ! A.style (stringValue "width: 100%; padding: 10px; background: #3498db; color: white; border: none; border-radius: 4px; cursor: pointer; font-size: 16px;") $ H.toHtml "Login"

        H.h2 ! A.style (stringValue "color: #27ae60; border-bottom: 2px solid #27ae60; padding-bottom: 10px; margin-top: 30px;") $ H.toHtml "Register"
        H.form ! Htmx.hxPost (stringValue "/register") ! Htmx.hxTarget (stringValue "body") $ do
          H.div ! A.style (stringValue "margin-bottom: 15px;") $ do
            H.label ! A.for (stringValue "reg_username") ! A.style (stringValue "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Username:"
            H.input ! A.type_ (stringValue "text") ! A.name (stringValue "username") ! A.id (stringValue "reg_username") ! A.required (stringValue "") ! A.style (stringValue "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
          H.div ! A.style (stringValue "margin-bottom: 15px;") $ do
            H.label ! A.for (stringValue "reg_email") ! A.style (stringValue "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Email:"
            H.input ! A.type_ (stringValue "email") ! A.name (stringValue "email") ! A.id (stringValue "reg_email") ! A.required (stringValue "") ! A.style (stringValue "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
          H.div ! A.style (stringValue "margin-bottom: 15px;") $ do
            H.label ! A.for (stringValue "reg_password") ! A.style (stringValue "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Password:"
            H.input ! A.type_ (stringValue "password") ! A.name (stringValue "password") ! A.id (stringValue "reg_password") ! A.required (stringValue "") ! A.style (stringValue "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
          H.button ! A.type_ (stringValue "submit") ! A.style (stringValue "width: 100%; padding: 10px; background: #27ae60; color: white; border: none; border-radius: 4px; cursor: pointer; font-size: 16px;") $ H.toHtml "Register"

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
              -- Return HTML that redirects to user page
              let redirectHtml = H.docTypeHtml $ do
                    H.head $ do
                      H.title $ H.toHtml "Login Successful"
                      H.script ! A.src (stringValue "https://unpkg.com/htmx.org@1.9.10") $ H.toHtml ""
                    H.body $ do
                      H.p $ H.toHtml "Login successful! Redirecting..."
                      H.script $
                        H.toHtml $
                          unlines
                            [ "// Redirect to user page",
                              "window.location.href = '/user';"
                            ]
              return $ addHeader sCookie redirectHtml
            Nothing -> throwError err500 {errBody = L8.pack "Failed to create authentication cookie"}
        Nothing -> do
          return $ noHeader $ H.docTypeHtml $ do
            H.head $ H.title $ H.toHtml "Login Failed"
            H.body $ do
              H.h1 $ H.toHtml "Login Failed"
              H.p $ H.toHtml "Invalid username or password."
              H.a ! A.href (stringValue "/") $ H.toHtml "Try again"
    _ -> do
      return $ noHeader $ H.docTypeHtml $ do
        H.head $ H.title $ H.toHtml "Login Error"
        H.body $ do
          H.h1 $ H.toHtml "Login Error"
          H.p $ H.toHtml "Missing username or password."
          H.a ! A.href (stringValue "/") $ H.toHtml "Try again"

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
        Left errorMsg -> do
          return $ H.docTypeHtml $ do
            H.head $ H.title $ H.toHtml "Registration Failed"
            H.body $ do
              H.h1 $ H.toHtml "Registration Failed"
              H.p $ H.toHtml errorMsg
              H.a ! A.href (stringValue "/") $ H.toHtml "Try again"
        Right () -> do
          result <- liftIO $ createUser dbConn regData
          case result of
            Left errorMsg -> do
              return $ H.docTypeHtml $ do
                H.head $ H.title $ H.toHtml "Registration Failed"
                H.body $ do
                  H.h1 $ H.toHtml "Registration Failed"
                  H.p $ H.toHtml errorMsg
                  H.a ! A.href (stringValue "/") $ H.toHtml "Try again"
            Right _ -> do
              return $ H.docTypeHtml $ do
                H.head $ H.title $ H.toHtml "Registration Successful"
                H.body $ do
                  H.h1 $ H.toHtml "Registration Successful"
                  H.p $ H.toHtml "You can now login with your credentials."
                  H.a ! A.href (stringValue "/") $ H.toHtml "Login"
    _ -> do
      return $ H.docTypeHtml $ do
        H.head $ H.title $ H.toHtml "Registration Error"
        H.body $ do
          H.h1 $ H.toHtml "Registration Error"
          H.p $ H.toHtml "Missing required fields."
          H.a ! A.href (stringValue "/") $ H.toHtml "Try again"

-- Logout handler - clears user session
logoutHandler :: Handler Html
logoutHandler = do
  return $ H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.httpEquiv (stringValue "refresh") ! A.content (stringValue "0;url=/")
      H.title $ H.toHtml "Logged Out"
    H.body $ do
      H.p $ H.toHtml "Logged out successfully. Redirecting to login..."

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
