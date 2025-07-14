{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import API.Handlers (getGameDataRow)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html)
import UI.HTMX (generateHTMXPage)

-- API type definition
type API =
  -- /
  Get '[HTML] Html
    -- /data
    :<|> "data" :> Get '[HTML] Html

-- Implement the server handlers
server :: Server API
server =
  return generateHTMXPage
    :<|> return getGameDataRow

-- Create the application
app :: Application
app = serve (Proxy :: Proxy API) server
