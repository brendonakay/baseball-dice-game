{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import API.Handlers
  ( advanceSeasonGameDataFrame,
    gameFrameHandler,
    gamesPageHandler,
    loginHandler,
    loginPageHandler,
    logoutHandler,
    nextSeasonGameHandler,
    personalCollectionPageHandler,
    registerHandler,
    seasonConfigPageHandler,
    seasonPageHandler,
    startNewSeasonHandler,
    startSeasonGameHandler,
    updateSeasonPlayerHandler,
    userPageHandler,
  )
import Database.SQLite.Simple (Connection)
import Servant
import Servant.Auth as SA
import Servant.Auth.Server (AuthResult (..), throwAll)
import qualified Servant.Auth.Server as SAS
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html)
import User.AuthenticatedUser (AuthenticatedUser)
import WaxBall.Season (SeasonRef)

-- API Types

-- Public API (no authentication required)
type PublicAPI =
  -- / (login page)
  Get '[HTML] Html
    -- Authentication
    :<|> "login"
      :> ReqBody '[FormUrlEncoded] [(String, String)]
      :> Post '[HTML] (Headers '[Header "Set-Cookie" SAS.SetCookie] Html)
    :<|> "register"
      :> ReqBody '[FormUrlEncoded] [(String, String)]
      :> Post '[HTML] Html
    :<|> "logout" :> Post '[HTML] Html

-- Protected API (authentication required)
type ProtectedAPI =
  Auth '[SA.Cookie] AuthenticatedUser
    :> ( -- Pages
         -- /user (user dashboard page)
         "user" :> Get '[HTML] Html
           :<|> "personal-collection" :> Get '[HTML] Html
           -- /season (dedicated season page)
           :<|> "season" :> Get '[HTML] Html
           -- /games (stub games page)
           :<|> "games" :> Get '[HTML] Html
           -- /game-frame (HTMX fragment for landing page game shell)
           :<|> "game-frame" :> Get '[HTML] Html
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

-- API Server Implementations

-- Public server handlers
publicServer :: Connection -> SAS.CookieSettings -> SAS.JWTSettings -> Server PublicAPI
publicServer dbConn cookieSettings jwtSettings =
  loginPageHandler
    -- Authentication
    :<|> loginHandler dbConn cookieSettings jwtSettings
    :<|> registerHandler dbConn
    :<|> logoutHandler

-- Protected server handlers
protectedServer ::
  Connection ->
  SeasonRef ->
  AuthResult AuthenticatedUser ->
  Server
    ( "user" :> Get '[HTML] Html
        :<|> "personal-collection" :> Get '[HTML] Html
        :<|> "season" :> Get '[HTML] Html
        :<|> "games" :> Get '[HTML] Html
        :<|> "game-frame" :> Get '[HTML] Html
        :<|> "start-season" :> Post '[HTML] Html
        :<|> "season-config" :> Get '[HTML] Html
        :<|> "start-game" :> Post '[HTML] Html
        :<|> "game-data" :> Get '[HTML] Html
        :<|> "next-game" :> Post '[HTML] Html
        :<|> "update-player" :> ReqBody '[FormUrlEncoded] [(String, String)] :> Post '[HTML] Html
    )
protectedServer dbConn seasonRef (Authenticated user) =
  userPageHandler user seasonRef
    :<|> personalCollectionPageHandler dbConn user
    :<|> seasonPageHandler user seasonRef
    :<|> gamesPageHandler user
    :<|> gameFrameHandler user seasonRef
    :<|> startNewSeasonHandler dbConn user seasonRef
    :<|> seasonConfigPageHandler user seasonRef
    :<|> startSeasonGameHandler seasonRef
    :<|> advanceSeasonGameDataFrame user seasonRef
    :<|> nextSeasonGameHandler user seasonRef
    :<|> (\formData -> updateSeasonPlayerHandler user seasonRef formData)
protectedServer _ _ _ = throwAll err401

-- Combined server
server :: Connection -> SeasonRef -> SAS.CookieSettings -> SAS.JWTSettings -> Server API
server dbConn seasonRef cookieSettings jwtSettings =
  publicServer dbConn cookieSettings jwtSettings
    :<|> protectedServer dbConn seasonRef

-- Create the application with a database connection and season state
-- Note: The context will be set up in Main.hs
app :: Context '[SAS.CookieSettings, SAS.JWTSettings] -> Connection -> SeasonRef -> Application
app ctx@(cookieSettings :. jwtSettings :. EmptyContext) dbConn seasonRef =
  serveWithContext (Proxy :: Proxy API) ctx (server dbConn seasonRef cookieSettings jwtSettings)
