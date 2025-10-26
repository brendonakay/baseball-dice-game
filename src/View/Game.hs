module View.Game where

import Data.Maybe (isJust)
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx as Htmx
import WaxBall.Game
  ( BasesState (..),
    GameState (..),
    Log (..),
    Player (..),
    StrikeAction (..),
    isGameOver,
  )

gameStateToHtml :: GameState -> Html
gameStateToHtml = gameFrameHtml

gameContainerHtml :: GameState -> Html
gameContainerHtml gs = H.div ! A.id (stringValue "game-container") $ do
  if not (isGameOver gs)
    then
      H.button
        ! Htmx.hxGet (stringValue "/data")
        ! Htmx.hxTarget (stringValue "#game-frame")
        ! Htmx.hxSwap (stringValue "outerHTML")
        ! A.style (stringValue "display: block; margin: 0 auto 20px auto; padding: 10px 20px; background: #3498db; color: white; border: none; border-radius: 5px; cursor: pointer;")
        $ H.toHtml "Continue Game"
    else H.div ! A.style (stringValue "text-align: center; margin: 20px;") $ do
      H.p ! A.style (stringValue "font-size: 1.2em; color: #2c3e50;") $ H.toHtml "Game Complete!"
      H.button
        ! A.onclick (stringValue "window.location.href='/user'")
        ! A.style (stringValue "padding: 10px 20px; background: #27ae60; color: white; border: none; border-radius: 5px; cursor: pointer; margin: 10px;")
        $ H.toHtml "Return to Dashboard"
  gameFrameHtml gs

completeGamePageHtml :: GameState -> Html
completeGamePageHtml gs = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset (stringValue "UTF-8")
    H.meta ! A.name (stringValue "viewport") ! A.content (stringValue "width=device-width, initial-scale=1.0")
    H.title $ H.toHtml "Baseball Game"
    H.script ! A.src (stringValue "https://unpkg.com/htmx.org@1.5.0") $ H.toHtml ""
    H.style $ H.toHtml gamePageCSS
  H.body $ do
    H.h1 ! A.style (stringValue "text-align: center; color: #2c3e50;") $ H.toHtml "Baseball Game"
    gameContainerHtml gs

gameFrameHtml :: GameState -> Html
gameFrameHtml gs = H.div ! A.id (stringValue "game-frame") ! A.class_ (stringValue "game-frame") $ do
  H.div ! A.class_ (stringValue "main-game-container") ! A.style (stringValue "display: flex; gap: 20px;") $ do
    H.div ! A.class_ (stringValue "game-content") ! A.style (stringValue "flex: 2;") $ do
      H.div ! A.class_ (stringValue "scoreboard") $ do
        H.div ! A.class_ (stringValue "score-section") $ do
          H.h3 (H.toHtml "Away")
          H.div ! A.class_ (stringValue "score") $ H.toHtml $ show $ awayScore gs
        H.div ! A.class_ (stringValue "score-section") $ do
          H.h3 (H.toHtml "Home")
          H.div ! A.class_ (stringValue "score") $ H.toHtml $ show $ homeScore gs

      H.div ! A.class_ (stringValue "game-info") $ do
        if isGameOver gs
          then do
            H.h2 ! A.style (stringValue "color: #e74c3c; font-size: 2.5em; text-align: center; margin: 20px 0;") $ H.toHtml "GAME OVER"
            H.h3 ! A.style (stringValue "text-align: center; color: #2c3e50;") $
              H.toHtml $
                let winner = if homeScore gs > awayScore gs then "Home" else "Away"
                    finalScore = show (awayScore gs) ++ "-" ++ show (homeScore gs)
                 in winner ++ " Team Wins! Final Score: " ++ finalScore
          else H.h2 $ H.toHtml $ "Inning " ++ show (inning gs) ++ " - " ++ show (halfInning gs)

      H.div ! A.class_ (stringValue "diamond-container") $ do
        H.div ! A.class_ (stringValue "diamond") $ H.toHtml ""
        H.div ! A.class_ (stringValue (if isJust (home $ bases gs) then "base home-plate occupied" else "base home-plate")) $ H.toHtml ""
        H.div ! A.class_ (stringValue (if isJust (first $ bases gs) then "base first-base occupied" else "base first-base")) $ H.toHtml ""
        H.div ! A.class_ (stringValue (if isJust (second $ bases gs) then "base second-base occupied" else "base second-base")) $ H.toHtml ""
        H.div ! A.class_ (stringValue (if isJust (third $ bases gs) then "base third-base occupied" else "base third-base")) $ H.toHtml ""

      H.div ! A.class_ (stringValue "batter-info") $ do
        H.h3 (H.toHtml "Current Batter")
        H.p $ H.toHtml $ maybe "None" (\player -> WaxBall.Game.name player ++ " (#" ++ show (WaxBall.Game.number player) ++ ")") $ currentBatter gs

      H.div ! A.class_ (stringValue "count") $ do
        H.div ! A.class_ (stringValue "count-item") $ do
          H.div ! A.class_ (stringValue "number") $ H.toHtml $ show $ balls gs
          H.div ! A.class_ (stringValue "label") $ H.toHtml "BALLS"
        H.div ! A.class_ (stringValue "count-item") $ do
          H.div ! A.class_ (stringValue "number") $ H.toHtml $ show $ strikes gs
          H.div ! A.class_ (stringValue "label") $ H.toHtml "STRIKES"
        H.div ! A.class_ (stringValue "count-item") $ do
          H.div ! A.class_ (stringValue "number") $ H.toHtml $ show $ outs gs
          H.div ! A.class_ (stringValue "label") $ H.toHtml "OUTS"

    H.div ! A.class_ (stringValue "game-log") ! A.style (stringValue "flex: 1; min-width: 300px;") $ do
      H.h3 ! A.style (stringValue "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;") $ H.toHtml "Game Log"
      if null (pitchLog gs)
        then H.p ! A.style (stringValue "color: #7f8c8d; font-style: italic;") $ H.toHtml "No pitches yet..."
        else
          H.div ! A.class_ (stringValue "log-entries") ! A.style (stringValue "max-height: 500px; overflow-y: auto; border: 1px solid #bdc3c7; border-radius: 5px;") $
            mapM_ renderLogEntry (pitchLog gs)

renderLogEntry :: Log -> Html
renderLogEntry logEntry =
  H.div ! A.class_ (stringValue "log-entry") ! A.style (stringValue "padding: 10px; border-bottom: 1px solid #ecf0f1; background: #f8f9fa;") $ do
    H.div ! A.style (stringValue "display: flex; justify-content: space-between; align-items: center;") $ do
      H.div ! A.style (stringValue "font-weight: bold; color: #2c3e50;") $ do
        H.toHtml $ maybe "Unknown Batter" (\player -> WaxBall.Game.name player ++ " (#" ++ show (WaxBall.Game.number player) ++ ")") (currentBatter_ logEntry)
        H.span ! A.style (stringValue "margin-left: 10px; font-weight: normal; color: #7f8c8d;") $
          H.toHtml $
            "Inning " ++ show (inning_ logEntry) ++ " - " ++ show (halfInning_ logEntry)
      H.div ! A.style (stringValue "font-weight: bold;") $ do
        case strikeAction_ logEntry of
          NoAction -> H.span ! A.style (stringValue "color: #95a5a6;") $ H.toHtml "Ball"
          CalledStrike -> H.span ! A.style (stringValue "color: #e74c3c;") $ H.toHtml "Strike"
          HitSingle -> H.span ! A.style (stringValue "color: #27ae60;") $ H.toHtml "Single"
          HitDouble -> H.span ! A.style (stringValue "color: #27ae60;") $ H.toHtml "Double"
          HitTriple -> H.span ! A.style (stringValue "color: #27ae60;") $ H.toHtml "Triple"
          HomeRun -> H.span ! A.style (stringValue "color: #f39c12;") $ H.toHtml "Home Run!"
          GroundOut -> H.span ! A.style (stringValue "color: #e74c3c;") $ H.toHtml "Ground Out"
          FlyOut -> H.span ! A.style (stringValue "color: #e74c3c;") $ H.toHtml "Fly Out"
          PopOut -> H.span ! A.style (stringValue "color: #e74c3c;") $ H.toHtml "Pop Out"
          FieldingError -> H.span ! A.style (stringValue "color: #f39c12;") $ H.toHtml "Fielding Error"
          HitByPitch -> H.span ! A.style (stringValue "color: #9b59b6;") $ H.toHtml "Hit By Pitch"
    H.div ! A.style (stringValue "margin-top: 5px; font-size: 0.9em; color: #7f8c8d;") $ do
      H.toHtml $
        "Score: "
          ++ show (awayScore_ logEntry)
          ++ "-"
          ++ show (homeScore_ logEntry)
          ++ " | Count: "
          ++ show (balls_ logEntry)
          ++ "-"
          ++ show (strikes_ logEntry)
          ++ " | Outs: "
          ++ show (outs_ logEntry)

gameRedirectHtml :: Html
gameRedirectHtml = do
  H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toHtml "Starting Game..."
      H.meta ! A.httpEquiv (stringValue "refresh") ! A.content (stringValue "0;url=/data")
    H.body $ do
      H.p $ H.toHtml "Starting game..."

autoAdvancingGamePageHtml :: GameState -> Html
autoAdvancingGamePageHtml gs = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset (stringValue "UTF-8")
    H.meta ! A.name (stringValue "viewport") ! A.content (stringValue "width=device-width, initial-scale=1.0")
    H.title $ H.toHtml "Baseball Game - Auto Play"
    H.script ! A.src (stringValue "https://unpkg.com/htmx.org@1.9.10") $ H.toHtml ""
    H.style $ H.toHtml gamePageCSS
  H.body $ do
    H.h1 ! A.style (stringValue "text-align: center; color: #2c3e50;") $ H.toHtml "Baseball Game - Auto Play"
    autoAdvancingGameContainerHtml gs

autoAdvancingGameContainerHtml :: GameState -> Html
autoAdvancingGameContainerHtml gs = H.div ! A.id (stringValue "game-container") $ do
  if not (isGameOver gs)
    then do
      H.div ! A.style (stringValue "text-align: center; margin: 20px;") $ do
        H.p ! A.style (stringValue "color: #2c3e50; font-size: 1.1em;") $ H.toHtml "Game is auto-advancing..."
        H.div
          ! Htmx.hxGet (stringValue "/game-data")
          ! Htmx.hxTarget (stringValue "#game-frame")
          ! Htmx.hxSwap (stringValue "outerHTML")
          ! Htmx.hxTrigger (stringValue "every 0.5s")
          $ H.toHtml ""
    else do
      H.div ! A.style (stringValue "text-align: center; margin: 20px;") $ do
        H.p ! A.style (stringValue "font-size: 1.2em; color: #2c3e50;") $ H.toHtml "Game Complete! Redirecting..."
        H.script $ H.toHtml "setTimeout(function() { window.location.href = '/user'; }, 1000);"
  gameFrameHtml gs

autoAdvancingGameFrameHtml :: GameState -> Html
autoAdvancingGameFrameHtml = gameFrameHtml

gameCompletionHtml :: GameState -> Html
gameCompletionHtml gs =
  H.div ! A.id (stringValue "game-frame") ! A.class_ (stringValue "game-frame") $ do
    H.div ! A.style (stringValue "text-align: center; padding: 40px; background: white; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);") $ do
      H.h2 ! A.style (stringValue "color: #27ae60; font-size: 2.5em; margin-bottom: 20px;") $ H.toHtml "Game Complete!"

      let finalHomeScore = homeScore gs
          finalAwayScore = awayScore gs
          winner = if finalHomeScore > finalAwayScore then "Home" else "Away"

      H.div ! A.style (stringValue "font-size: 1.5em; margin: 20px 0; color: #2c3e50;") $ do
        H.strong $ H.toHtml $ winner ++ " Team Wins!"

      H.div ! A.style (stringValue "font-size: 1.3em; margin: 20px 0; color: #34495e;") $ do
        H.toHtml $ "Final Score: Away " ++ show finalAwayScore ++ " - Home " ++ show finalHomeScore

      H.div ! A.style (stringValue "font-size: 1.1em; margin: 20px 0; color: #7f8c8d;") $ do
        H.toHtml $ "Innings Played: " ++ show (inning gs)

      H.p ! A.style (stringValue "font-size: 1.1em; color: #95a5a6; margin-top: 30px;") $
        H.toHtml "Redirecting to dashboard in 2 seconds..."

      H.script $ H.toHtml "setTimeout(function() { window.location.href = '/user'; }, 2000);"

gamePageCSS :: String
gamePageCSS =
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
