{-# LANGUAGE OverloadedStrings #-}

module UI.HTMX where

import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx as Htmx

-- Function to generate a simple HTML page with HTMX
generateHTMXPage :: Html
generateHTMXPage = docTypeHtml $ do
  H.head $ do
    H.title "Game Data"
    H.script ! A.src "https://unpkg.com/htmx.org@1.5.0" $ ""
  body $ do
    H.h1 "Game Data"
    H.table ! A.id "data-table" $ do
      H.thead $ do
        H.tr $ do
          H.th "Field1"
          H.th "Field2"
          H.th "Field3"
      H.tbody ! Htmx.hxGet "/api/game-data" ! Htmx.hxTrigger "load" $ ""

-- This function can be used to render the HTML page
renderPage :: IO ()
renderPage = putStrLn $ renderHtml generateHTMXPage
