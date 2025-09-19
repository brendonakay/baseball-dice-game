module API.Handlers where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection)
import Servant
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (stringValue)
import Text.Read (readMaybe)

import User.Auth (LoginCredentials (..), RegisterData (..), authenticateUser, createUser, validateRegistration)

import User.AuthenticatedUser (AuthenticatedUser, UserRef)

import View.HTMX (autoAdvancingGameFrameHtml, autoAdvancingGamePageHtml, gameCompletionHtml, seasonConfigPageToHtml, seasonPageToHtml, updatePlayerAtIndex)
import View.PersonalCollection (personalCollectionPageToHtml)
import View.User (userPageToHtml)
import WaxBall.Game (Player (..), isGameOver)
import WaxBall.Season (GameResult (..), SeasonRef, SeasonState (..), getCurrentSeasonState, newSeasonState, runAdvanceCurrentGame, runRecordGameResult, runStartNextGame)

-- Helper function to convert strings to AttributeValue
str :: String -> H.AttributeValue
str = stringValue

-- Login page handler - shows login/register form
loginPageHandler :: Handler Html
loginPageHandler = do
  return $ H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toHtml "Baseball Dice Game - Login"
      H.meta ! A.charset (str "utf-8")
      H.meta ! A.name (str "viewport") ! A.content (str "width=device-width, initial-scale=1")
    H.body ! A.style (str "background: #f5f5f5; font-family: Arial, sans-serif; margin: 0; padding: 0; min-height: 100vh;") $ do
      H.div ! A.style (str "max-width: 400px; margin: 50px auto; padding: 20px; background: white; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);") $ do
        H.h1 ! A.style (str "text-align: center; color: #2c3e50; margin-bottom: 30px;") $ H.toHtml "Baseball Dice Game"
        
        H.h2 ! A.style (str "color: #3498db; border-bottom: 2px solid #3498db; padding-bottom: 10px;") $ H.toHtml "Login"
        H.form ! A.action (str "/login") ! A.method (str "post") $ do
          H.div ! A.style (str "margin-bottom: 15px;") $ do
            H.label ! A.for (str "username") ! A.style (str "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Username:"
            H.input ! A.type_ (str "text") ! A.name (str "username") ! A.id (str "username") ! A.required (str "") ! A.style (str "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
          H.div ! A.style (str "margin-bottom: 15px;") $ do  
            H.label ! A.for (str "password") ! A.style (str "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Password:"
            H.input ! A.type_ (str "password") ! A.name (str "password") ! A.id (str "password") ! A.required (str "") ! A.style (str "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
          H.button ! A.type_ (str "submit") ! A.style (str "width: 100%; padding: 10px; background: #3498db; color: white; border: none; border-radius: 4px; cursor: pointer; font-size: 16px;") $ H.toHtml "Login"
        
        H.h2 ! A.style (str "color: #27ae60; border-bottom: 2px solid #27ae60; padding-bottom: 10px; margin-top: 30px;") $ H.toHtml "Register"
        H.form ! A.action (str "/register") ! A.method (str "post") $ do
          H.div ! A.style (str "margin-bottom: 15px;") $ do
            H.label ! A.for (str "reg_username") ! A.style (str "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Username:"
            H.input ! A.type_ (str "text") ! A.name (str "username") ! A.id (str "reg_username") ! A.required (str "") ! A.style (str "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
          H.div ! A.style (str "margin-bottom: 15px;") $ do
            H.label ! A.for (str "reg_email") ! A.style (str "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Email:"
            H.input ! A.type_ (str "email") ! A.name (str "email") ! A.id (str "reg_email") ! A.required (str "") ! A.style (str "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
          H.div ! A.style (str "margin-bottom: 15px;") $ do  
            H.label ! A.for (str "reg_password") ! A.style (str "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Password:"
            H.input ! A.type_ (str "password") ! A.name (str "password") ! A.id (str "reg_password") ! A.required (str "") ! A.style (str "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
          H.button ! A.type_ (str "submit") ! A.style (str "width: 100%; padding: 10px; background: #27ae60; color: white; border: none; border-radius: 4px; cursor: pointer; font-size: 16px;") $ H.toHtml "Register"

-- Login handler - processes login form
loginHandler :: Connection -> UserRef -> [(String, String)] -> Handler Html
loginHandler dbConn userRef formData = do
  let getFormValue key = T.pack <$> lookup key formData
      username = getFormValue "username"
      password = getFormValue "password"
  case (username, password) of
    (Just u, Just p) -> do
      let creds = LoginCredentials u p
      maybeUser <- liftIO $ authenticateUser dbConn creds
      case maybeUser of
        Just user -> do
          liftIO $ writeIORef userRef (Just user)
          return $ H.docTypeHtml $ do
            H.head $ do
              H.meta ! A.httpEquiv (stringValue "refresh") ! A.content (stringValue "0;url=/user")
              H.title $ H.toHtml "Login Successful"
            H.body $ do
              H.p $ H.toHtml "Login successful! Redirecting..."
        Nothing -> do
          return $ H.docTypeHtml $ do
            H.head $ H.title $ H.toHtml "Login Failed"
            H.body $ do
              H.h1 $ H.toHtml "Login Failed"
              H.p $ H.toHtml "Invalid username or password."
              H.a ! A.href (str "/") $ H.toHtml "Try again"
    _ -> do
      return $ H.docTypeHtml $ do
        H.head $ H.title $ H.toHtml "Login Error"
        H.body $ do
          H.h1 $ H.toHtml "Login Error"
          H.p $ H.toHtml "Missing username or password."
          H.a ! A.href (str "/") $ H.toHtml "Try again"

-- Register handler - processes registration form
registerHandler :: Connection -> UserRef -> [(String, String)] -> Handler Html
registerHandler dbConn userRef formData = do
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
              H.a ! A.href (str "/") $ H.toHtml "Try again"
        Right () -> do
          result <- liftIO $ createUser dbConn regData
          case result of
            Left errorMsg -> do
              return $ H.docTypeHtml $ do
                H.head $ H.title $ H.toHtml "Registration Failed"
                H.body $ do
                  H.h1 $ H.toHtml "Registration Failed"
                  H.p $ H.toHtml errorMsg
                  H.a ! A.href (str "/") $ H.toHtml "Try again"
            Right _ -> do
              return $ H.docTypeHtml $ do
                H.head $ H.title $ H.toHtml "Registration Successful"
                H.body $ do
                  H.h1 $ H.toHtml "Registration Successful"
                  H.p $ H.toHtml "You can now login with your credentials."
                  H.a ! A.href (str "/") $ H.toHtml "Login"
    _ -> do
      return $ H.docTypeHtml $ do
        H.head $ H.title $ H.toHtml "Registration Error"
        H.body $ do
          H.h1 $ H.toHtml "Registration Error"
          H.p $ H.toHtml "Missing required fields."
          H.a ! A.href (str "/") $ H.toHtml "Try again"

-- Logout handler - clears user session
logoutHandler :: UserRef -> Handler Html
logoutHandler userRef = do
  liftIO $ writeIORef userRef Nothing
  return $ H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.httpEquiv (stringValue "refresh") ! A.content (stringValue "0;url=/")
      H.title $ H.toHtml "Logged Out"
    H.body $ do
      H.p $ H.toHtml "Logged out successfully. Redirecting to login..."

-- User page handler - user dashboard with season info
userPageHandler :: UserRef -> SeasonRef -> Handler Html
userPageHandler userRef seasonRef = do
  maybeUser <- liftIO $ readIORef userRef
  case maybeUser of
    Nothing -> do
      return $ H.docTypeHtml $ do
        H.head $ do
          H.meta ! A.httpEquiv (stringValue "refresh") ! A.content (stringValue "0;url=/")
          H.title $ H.toHtml "Please Login"
        H.body $ do
          H.p $ H.toHtml "Please login to access your dashboard. Redirecting..."
    Just user -> do
      seasonState <- liftIO $ getCurrentSeasonState seasonRef
      return $ userPageToHtml user seasonState

-- Personal collection page handler - displays user's card collection
personalCollectionPageHandler :: UserRef -> SeasonRef -> Handler Html
personalCollectionPageHandler userRef _ = do
  maybeUser <- liftIO $ readIORef userRef
  case maybeUser of
    Nothing -> do
      return $ H.docTypeHtml $ do
        H.head $ do
          H.meta ! A.httpEquiv (stringValue "refresh") ! A.content (stringValue "0;url=/")
          H.title $ H.toHtml "Please Login"
        H.body $ do
          H.p $ H.toHtml "Please login to access your collection. Redirecting..."
    Just user -> do
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
advanceSeasonGameDataFrame :: UserRef -> SeasonRef -> Handler Html
advanceSeasonGameDataFrame userRef seasonRef = do
  -- Advance the current game by one step, maintaining all game state including pitch log
  maybeGameState <- liftIO $ runAdvanceCurrentGame seasonRef
  case maybeGameState of
    Nothing -> do
      -- No current game, show game completion with stats from most recent game
      seasonState <- liftIO $ getCurrentSeasonState seasonRef
      case gameResults seasonState of
        [] -> do
          -- No games completed yet, fallback to user page
          maybeUser <- liftIO $ readIORef userRef
          case maybeUser of
            Nothing -> return $ H.docTypeHtml $ do
              H.head $ do
                H.meta ! A.httpEquiv (stringValue "refresh") ! A.content (stringValue "0;url=/")
                H.title $ H.toHtml "Please Login"
              H.body $ do
                H.p $ H.toHtml "Please login to continue. Redirecting..."
            Just user -> return $ userPageToHtml user seasonState
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
