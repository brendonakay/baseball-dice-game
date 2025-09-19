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
import Database.SQLite.Simple (Connection)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html)
import User.AuthenticatedUser (UserRef)
import WaxBall.Season (SeasonRef)

-- API type definition for season-based flow
type API =
  -- / (login page)
  Get '[HTML] Html
    -- Authentication
    :<|> "login" :> ReqBody '[FormUrlEncoded] [(String, String)] :> Post '[HTML] Html
    :<|> "register" :> ReqBody '[FormUrlEncoded] [(String, String)] :> Post '[HTML] Html
    :<|> "logout" :> Post '[HTML] Html
    -- Pages
    -- /user (user dashboard page)
    :<|> "user" :> Get '[HTML] Html
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
    :<|> "update-player" :> ReqBody '[FormUrlEncoded] [(String, String)] :> Post '[HTML] Html

-- Implement the server handlers
server :: Connection -> UserRef -> SeasonRef -> Server API
server dbConn userRef seasonRef =
  loginPageHandler
    :<|> loginHandler dbConn userRef
    :<|> registerHandler dbConn userRef
    :<|> logoutHandler userRef
    :<|> userPageHandler userRef seasonRef
    :<|> personalCollectionPageHandler userRef seasonRef
    :<|> startNewSeasonHandler seasonRef
    :<|> seasonConfigPageHandler seasonRef
    :<|> startSeasonGameHandler seasonRef
    :<|> advanceSeasonGameDataFrame userRef seasonRef
    :<|> nextSeasonGameHandler seasonRef
    :<|> updateSeasonPlayerHandler seasonRef

-- Create the application with database connection, user and season state
app :: Connection -> UserRef -> SeasonRef -> Application
app dbConn userRef seasonRef = serve (Proxy :: Proxy API) (server dbConn userRef seasonRef)
