{-# LANGUAGE OverloadedStrings #-}

module UI.HTMX where

import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx as Htmx

generateHTMXPage :: Html
generateHTMXPage = docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset "UTF-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
    H.title "Game Data"
    H.script ! A.src "https://unpkg.com/htmx.org@1.5.0" $ ""
  H.body $ do
    H.h1 "Game Data"
    H.button
      ! Htmx.hxGet "/data"
      ! Htmx.hxTarget "#game-data-table-body"
      ! Htmx.hxSwap "beforeend"
      $ "Load Game Data"
    H.table ! A.style "border: 1px solid black;" $ do
      H.thead $ H.tr $ do
        H.th "Home Score" ! A.style "border: 1px solid black;"
        H.th "Away Score" ! A.style "border: 1px solid black;"
        H.th "Inning" ! A.style "border: 1px solid black;"
        H.th "Half Inning" ! A.style "border: 1px solid black;"
        H.th "Current Batter" ! A.style "border: 1px solid black;"
        H.th "Balls" ! A.style "border: 1px solid black;"
        H.th "Strikes" ! A.style "border: 1px solid black;"
        H.th "Outs" ! A.style "border: 1px solid black;"
      H.tbody ! A.id "game-data-table-body" $ return ()

-- This function can be used to render the HTML page
renderPage :: IO ()
renderPage = putStrLn $ renderHtml generateHTMXPage
