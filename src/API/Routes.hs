{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import API.Handlers (advanceGameDataFrame, configPageHandler, startGameHandler, updatePlayerHandler)
import Game.State (GameRef)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html)
import UI.HTMX (generateHTMXPage)

-- API type definition
type API =
  -- /
  Get '[HTML] Html
    -- /data (advances game state)
    :<|> "data" :> Get '[HTML] Html
    -- /config (team configuration page)
    :<|> "config" :> Get '[HTML] Html
    -- /start-game (starts game with configured teams)
    :<|> "start-game" :> Post '[HTML] Html
    -- /update-player (updates a single player)
    :<|> "update-player" :> ReqBody '[FormUrlEncoded] [(String, String)] :> Post '[HTML] Html

-- Implement the server handlers
server :: GameRef -> Server API
server gameRef =
  configPageHandler gameRef
    :<|> advanceGameDataFrame gameRef
    :<|> configPageHandler gameRef
    :<|> startGameHandler gameRef
    :<|> updatePlayerHandler gameRef

-- Create the application with game state
app :: GameRef -> Application
app gameRef = serve (Proxy :: Proxy API) (server gameRef)
