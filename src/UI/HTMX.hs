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
    H.title "Baseball Game"
    H.script ! A.src "https://unpkg.com/htmx.org@1.5.0" $ ""
    H.style $
      H.toHtml $
        unlines
          [ ".game-frame { font-family: Arial, sans-serif; max-width: 800px; margin: 20px auto; }",
            ".scoreboard { display: flex; justify-content: space-between; background: #2c3e50; color: white; padding: 15px; border-radius: 8px; margin-bottom: 20px; }",
            ".score-section h3 { margin: 0 0 5px 0; }",
            ".score-section .score { font-size: 2em; font-weight: bold; }",
            ".game-info { text-align: center; margin-bottom: 20px; }",
            ".diamond-container { position: relative; width: 300px; height: 300px; margin: 0 auto; }",
            ".diamond { width: 200px; height: 200px; background: #8B4513; transform: rotate(45deg); position: absolute; top: 50px; left: 50px; border-radius: 15px; }",
            ".base { position: absolute; width: 20px; height: 20px; background: white; border: 2px solid #333; }",
            ".base.occupied { background: #ff6b35; }",
            ".first-base { top: 140px; right: 40px; }",
            ".second-base { top: 40px; right: 140px; }",
            ".third-base { top: 140px; left: 40px; }",
            ".home-plate { bottom: 40px; left: 140px; border-radius: 50%; }",
            ".batter-info { text-align: center; margin: 20px 0; padding: 15px; background: #ecf0f1; border-radius: 8px; }",
            ".count { display: flex; justify-content: center; gap: 30px; margin-top: 20px; }",
            ".count-item { text-align: center; }",
            ".count-item .number { font-size: 2em; font-weight: bold; color: #2c3e50; }",
            ".count-item .label { font-size: 0.9em; color: #7f8c8d; }",
            ".game-log { background: white; border-radius: 8px; padding: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }",
            ".log-entries { scrollbar-width: thin; scrollbar-color: #bdc3c7 #f8f9fa; }",
            ".log-entries::-webkit-scrollbar { width: 8px; }",
            ".log-entries::-webkit-scrollbar-track { background: #f8f9fa; }",
            ".log-entries::-webkit-scrollbar-thumb { background: #bdc3c7; border-radius: 4px; }"
          ]
  H.body $ do
    H.h1 ! A.style "text-align: center; color: #2c3e50;" $ "Baseball Game"
    H.button
      ! Htmx.hxGet "/data"
      ! Htmx.hxTarget "#game-frame"
      ! Htmx.hxSwap "outerHTML"
      ! A.style "display: block; margin: 0 auto 20px auto; padding: 10px 20px; background: #3498db; color: white; border: none; border-radius: 5px; cursor: pointer;"
      $ "Refresh Game Data"
    H.div ! A.id "game-frame" $ H.div ! A.class_ "game-frame" $ H.p "Loading game data..."

-- This function can be used to render the HTML page
renderPage :: IO ()
renderPage = putStrLn $ renderHtml generateHTMXPage
