{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import API.Handlers (advanceSeasonGameDataFrame, finishSeasonGameHandler, nextSeasonGameHandler, seasonConfigPageHandler, seasonPageHandler, startNewSeasonHandler, startSeasonGameHandler, updateSeasonPlayerHandler)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html)
import WaxBall.Season (SeasonRef)

-- API type definition for season-based flow
type API =
  -- / (main season page)
  Get '[HTML] Html
    -- /start-season (start a new 10-game season)
    :<|> "start-season" :> Post '[HTML] Html
    -- /season-config (team configuration page for current season)
    :<|> "season-config" :> Get '[HTML] Html
    -- /start-game (starts next game in season with configured teams)
    :<|> "start-game" :> Post '[HTML] Html
    -- /game-data (auto-advances current game state)
    :<|> "game-data" :> Get '[HTML] Html
    -- /finish-game (called when game is complete, returns to season page)
    :<|> "finish-game" :> Post '[HTML] Html
    -- /next-game (starts the next game in the season)
    :<|> "next-game" :> Post '[HTML] Html
    -- /update-player (updates a single player in season)
    :<|> "update-player" :> ReqBody '[FormUrlEncoded] [(String, String)] :> Post '[HTML] Html

-- Implement the server handlers
server :: SeasonRef -> Server API
server seasonRef =
  seasonPageHandler seasonRef
    :<|> startNewSeasonHandler seasonRef
    :<|> seasonConfigPageHandler seasonRef
    :<|> startSeasonGameHandler seasonRef
    :<|> advanceSeasonGameDataFrame seasonRef
    :<|> finishSeasonGameHandler seasonRef
    :<|> nextSeasonGameHandler seasonRef
    :<|> updateSeasonPlayerHandler seasonRef

-- Create the application with season state
app :: SeasonRef -> Application
app seasonRef = serve (Proxy :: Proxy API) (server seasonRef)
