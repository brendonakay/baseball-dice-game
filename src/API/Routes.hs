{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html)
import UI.HTMX (generateHTMXPage)

-- Define the API type
type API =
  Get '[HTML] Html
    :<|> "data" :> Get '[HTML] Html

-- Implement the server handlers
server :: Server API
server =
  return generateHTMXPage
    :<|> return generateHTMXPage

-- Create the application
app :: Application
app = serve (Proxy :: Proxy API) server
