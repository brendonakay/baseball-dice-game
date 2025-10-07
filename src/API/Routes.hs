{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import API.Handlers
  ( advanceSeasonGameDataFrame,
    loginHandler,
    loginPageHandler,
    logoutHandler,
    nextSeasonGameHandler,
    personalCollectionPageHandler,
    registerHandler,
    seasonConfigPageHandler,
    startNewSeasonHandler,
    startSeasonGameHandler,
    updateSeasonPlayerHandler,
    userPageHandler,
  )
import Control.Monad.IO.Class (liftIO)
import Data.IORef (writeIORef)
import Database.SQLite.Simple (Connection)
import Servant
import Servant.Auth as SA
import Servant.Auth.Server (AuthResult(..), throwAll)
import qualified Servant.Auth.Server as SAS
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html)
import User.AuthenticatedUser (AuthenticatedUser, UserRef)
import WaxBall.Season (SeasonRef)

-- Public API (no authentication required)
type PublicAPI =
  -- / (login page)
  Get '[HTML] Html
    -- Authentication
    :<|> "login"
      :> ReqBody '[FormUrlEncoded] [(String, String)]
      :> Post '[HTML] Html
    :<|> "register"
      :> ReqBody '[FormUrlEncoded] [(String, String)]
      :> Post '[HTML] Html
    :<|> "logout" :> Post '[HTML] Html

-- Protected API (authentication required)  
type ProtectedAPI = Auth '[SA.JWT] AuthenticatedUser :> (
    -- Pages
    -- /user (user dashboard page)
    "user" :> Get '[HTML] Html
    :<|> "personal-collection" :> Get '[HTML] Html
    --
    -- Containers & Data
    -- /start-season (start a new 10-game season)
    :<|> "start-season" :> Post '[HTML] Html
    -- /season-config (team configuration page for current season)
    :<|> "season-config" :> Get '[HTML] Html
    -- /start-game (starts next game in season with configured teams)
    :<|> "start-game" :> Post '[HTML] Html
    -- /game-data (auto-advances current game state)
    :<|> "game-data" :> Get '[HTML] Html
    -- /next-game (starts the next game in the season)
    :<|> "next-game" :> Post '[HTML] Html
    -- /update-player (updates a single player in season)
    :<|> "update-player"
      :> ReqBody '[FormUrlEncoded] [(String, String)]
      :> Post '[HTML] Html
  )

-- Combined API
type API = PublicAPI :<|> ProtectedAPI

-- Public server handlers
publicServer :: Connection -> UserRef -> Server PublicAPI
publicServer dbConn userRef =
  loginPageHandler
    -- Authentication
    :<|> loginHandler dbConn userRef
    :<|> registerHandler dbConn userRef
    :<|> logoutHandler userRef

-- Protected server handlers
protectedServer :: UserRef -> SeasonRef -> AuthResult AuthenticatedUser -> Server (
    "user" :> Get '[HTML] Html
    :<|> "personal-collection" :> Get '[HTML] Html
    :<|> "start-season" :> Post '[HTML] Html
    :<|> "season-config" :> Get '[HTML] Html
    :<|> "start-game" :> Post '[HTML] Html
    :<|> "game-data" :> Get '[HTML] Html
    :<|> "next-game" :> Post '[HTML] Html
    :<|> "update-player" :> ReqBody '[FormUrlEncoded] [(String, String)] :> Post '[HTML] Html
  )
protectedServer userRef seasonRef (Authenticated user) =
  let -- Helper to set user before calling handler
      withUser handler = do
        liftIO $ writeIORef userRef (Just user)
        handler
  in     withUser (userPageHandler userRef seasonRef)
    :<|> withUser (personalCollectionPageHandler userRef seasonRef)
    :<|> withUser (startNewSeasonHandler seasonRef)
    :<|> withUser (seasonConfigPageHandler seasonRef)
    :<|> withUser (startSeasonGameHandler seasonRef)
    :<|> withUser (advanceSeasonGameDataFrame userRef seasonRef)
    :<|> withUser (nextSeasonGameHandler seasonRef)
    :<|> \formData -> do
           liftIO $ writeIORef userRef (Just user)
           updateSeasonPlayerHandler seasonRef formData
protectedServer _ _ _ = throwAll err401

-- Combined server
server :: Connection -> UserRef -> SeasonRef -> Server API
server dbConn userRef seasonRef =
  publicServer dbConn userRef :<|> protectedServer userRef seasonRef

-- Create the application with database connection, user and season state
-- Note: The context will be set up in Main.hs
app :: Context '[SAS.JWTSettings, SAS.CookieSettings] -> Connection -> UserRef -> SeasonRef -> Application
app ctx dbConn userRef seasonRef = serveWithContext (Proxy :: Proxy API) ctx (server dbConn userRef seasonRef)
