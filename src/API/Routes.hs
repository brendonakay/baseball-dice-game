{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import API.Handlers (advanceGameDataFrame)
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

-- Implement the server handlers
server :: GameRef -> Server API
server gameRef =
  return generateHTMXPage
    :<|> advanceGameDataFrame gameRef

-- Create the application with game state
app :: GameRef -> Application
app gameRef = serve (Proxy :: Proxy API) (server gameRef)
