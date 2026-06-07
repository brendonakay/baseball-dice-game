module View.Game where

import Data.Maybe (isJust)
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx as Htmx
import User.AuthenticatedUser (AuthenticatedUser)
import View.Layout (mainLayout)
import WaxBall.Game
  ( BasesState (..),
    GameState (..),
    Log (..),
    Player (..),
    StrikeAction (..),
    isGameOver,
  )

-- Fragment for HTMX /game-frame endpoint when a game is active
-- Returns autoAdvancingGameContainerHtml — no docTypeHtml wrapper
activeGameFragment :: GameState -> Html
activeGameFragment = autoAdvancingGameContainerHtml

-- Full page wrapping autoAdvancingGameContainerHtml (used for direct navigation)
autoAdvancingGamePageHtml :: AuthenticatedUser -> GameState -> Html
autoAdvancingGamePageHtml user gs =
  mainLayout user "Game" $
    H.div $ do
      H.h1 ! A.class_ (stringValue "page-title") $ H.toHtml "Baseball Game"
      autoAdvancingGameContainerHtml gs

autoAdvancingGameContainerHtml :: GameState -> Html
autoAdvancingGameContainerHtml gs = H.div ! A.id (stringValue "game-container") $ do
  if not (isGameOver gs)
    then do
      H.div ! A.style (stringValue "text-align: center; padding: 8px; margin-bottom: 8px;") $ do
        H.p
          ! A.style (stringValue "color: #8b1a1a; font-family: 'Arial Black', sans-serif; font-size: 0.75rem; text-transform: uppercase; letter-spacing: 2px;")
          $ H.toHtml "Auto-advancing..."
        H.div
          ! Htmx.hxGet (stringValue "/game-data")
          ! Htmx.hxTarget (stringValue "#game-frame")
          ! Htmx.hxSwap (stringValue "outerHTML")
          ! Htmx.hxTrigger (stringValue "every 0.5s")
          $ H.toHtml ""
    else do
      H.div ! A.style (stringValue "text-align: center; padding: 8px; margin-bottom: 8px;") $ do
        H.p
          ! A.style (stringValue "font-size: 0.9rem; color: #1a4427; font-family: 'Arial Black', sans-serif; text-transform: uppercase; letter-spacing: 2px;")
          $ H.toHtml "Game Complete! Redirecting..."
        H.script $ H.toHtml "setTimeout(function() { window.location.href = '/user'; }, 1000);"
  gameFrameHtml gs

autoAdvancingGameFrameHtml :: GameState -> Html
autoAdvancingGameFrameHtml = gameFrameHtml

gameCompletionHtml :: GameState -> Html
gameCompletionHtml gs =
  H.div ! A.id (stringValue "game-frame") ! A.class_ (stringValue "game-frame") $ do
    H.div
      ! A.class_ (stringValue "panel")
      ! A.style (stringValue "text-align: center; padding: 40px;")
      $ do
        H.h2
          ! A.style (stringValue "font-family: 'Arial Black', sans-serif; color: #1a4427; font-size: 2rem; text-transform: uppercase; letter-spacing: 3px; margin-bottom: 20px;")
          $ H.toHtml "Game Complete!"
        let finalHomeScore = homeScore gs
            finalAwayScore = awayScore gs
            winner = if finalHomeScore > finalAwayScore then "Home" else "Away"
        H.div
          ! A.style (stringValue "font-family: 'Arial Black', sans-serif; font-size: 1.4rem; color: #1a2744; text-transform: uppercase; letter-spacing: 2px; margin: 16px 0;")
          $ H.toHtml
          $ winner ++ " Team Wins!"
        H.div
          ! A.style (stringValue "font-size: 1.2rem; color: #8b1a1a; margin: 16px 0;")
          $ H.toHtml
          $ "Final: Away " ++ show finalAwayScore ++ " — Home " ++ show finalHomeScore
        H.div
          ! A.style (stringValue "font-size: 0.9rem; color: #1a2744; margin: 16px 0;")
          $ H.toHtml
          $ "Innings: " ++ show (inning gs)
        H.p
          ! A.style (stringValue "font-size: 0.75rem; color: #8b1a1a; font-family: 'Arial Black', sans-serif; text-transform: uppercase; letter-spacing: 2px; margin-top: 24px;")
          $ H.toHtml "Returning to dashboard..."
        H.script $ H.toHtml "setTimeout(function() { window.location.href = '/user'; }, 2000);"

gameFrameHtml :: GameState -> Html
gameFrameHtml gs = H.div ! A.id (stringValue "game-frame") ! A.class_ (stringValue "game-frame") $ do
  H.div ! A.style (stringValue "display: flex; gap: 20px; flex-wrap: wrap;") $ do
    H.div ! A.style (stringValue "flex: 2; min-width: 280px;") $ do
      -- Scoreboard
      H.div ! A.class_ (stringValue "scoreboard") $ do
        H.div ! A.class_ (stringValue "score-section") $ do
          H.h3 $ H.toHtml "Away"
          H.div ! A.class_ (stringValue "score") $ H.toHtml $ show $ awayScore gs
        H.div ! A.class_ (stringValue "score-section") $ do
          H.h3 $ H.toHtml "Home"
          H.div ! A.class_ (stringValue "score") $ H.toHtml $ show $ homeScore gs

      -- Inning / Game Over
      H.div ! A.class_ (stringValue "game-info") $
        if isGameOver gs
          then
            H.toHtml $
              let winner = if homeScore gs > awayScore gs then "Home" else "Away"
               in "GAME OVER — " ++ winner ++ " Wins"
          else H.toHtml $ "Inning " ++ show (inning gs) ++ " — " ++ show (halfInning gs)

      -- Diamond
      H.div ! A.class_ (stringValue "diamond-container") $ do
        H.div ! A.class_ (stringValue "diamond") $ H.toHtml ""
        H.div ! A.class_ (stringValue (if isJust (home $ bases gs) then "base home-plate occupied" else "base home-plate")) $ H.toHtml ""
        H.div ! A.class_ (stringValue (if isJust (first $ bases gs) then "base first-base occupied" else "base first-base")) $ H.toHtml ""
        H.div ! A.class_ (stringValue (if isJust (second $ bases gs) then "base second-base occupied" else "base second-base")) $ H.toHtml ""
        H.div ! A.class_ (stringValue (if isJust (third $ bases gs) then "base third-base occupied" else "base third-base")) $ H.toHtml ""

      -- Batter
      H.div ! A.class_ (stringValue "batter-info") $
        H.toHtml $
          maybe "—" (\p -> WaxBall.Game.name p ++ " (#" ++ show (WaxBall.Game.number p) ++ ")") $
            currentBatter gs

      -- Count
      H.div ! A.class_ (stringValue "count") $ do
        H.div ! A.class_ (stringValue "count-item") $ do
          H.div ! A.class_ (stringValue "number") $ H.toHtml $ show $ balls gs
          H.div ! A.class_ (stringValue "label") $ H.toHtml "Balls"
        H.div ! A.class_ (stringValue "count-item") $ do
          H.div ! A.class_ (stringValue "number") $ H.toHtml $ show $ strikes gs
          H.div ! A.class_ (stringValue "label") $ H.toHtml "Strikes"
        H.div ! A.class_ (stringValue "count-item") $ do
          H.div ! A.class_ (stringValue "number") $ H.toHtml $ show $ outs gs
          H.div ! A.class_ (stringValue "label") $ H.toHtml "Outs"

    -- Game log
    H.div ! A.class_ (stringValue "game-log") ! A.style (stringValue "flex: 1; min-width: 260px;") $ do
      H.h3
        ! A.style (stringValue "font-family: 'Arial Black', sans-serif; font-size: 0.75rem; text-transform: uppercase; letter-spacing: 2px; color: #1a2744; border-bottom: 2px solid #c9a227; padding-bottom: 8px; margin-bottom: 10px;")
        $ H.toHtml "Game Log"
      if null (pitchLog gs)
        then
          H.p
            ! A.style (stringValue "font-family: 'Arial Black', sans-serif; font-size: 0.65rem; color: #8b1a1a; text-transform: uppercase; letter-spacing: 1px;")
            $ H.toHtml "No pitches yet..."
        else
          H.div ! A.class_ (stringValue "log-entries") $
            mapM_ renderLogEntry (pitchLog gs)

renderLogEntry :: Log -> Html
renderLogEntry logEntry =
  H.div
    ! A.style (stringValue "padding: 8px; border-bottom: 1px solid #e8dcc0; font-size: 0.8rem;")
    $ do
      H.div ! A.style (stringValue "display: flex; justify-content: space-between; align-items: center;") $ do
        H.div ! A.style (stringValue "font-family: 'Arial Black', sans-serif; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 1px; color: #1a2744;") $ do
          H.toHtml $
            maybe "Unknown" (\p -> WaxBall.Game.name p) (currentBatter_ logEntry)
          H.span
            ! A.style (stringValue "margin-left: 8px; font-weight: normal; color: #8b1a1a; font-family: Georgia, serif; font-size: 0.7rem; text-transform: none; letter-spacing: 0;")
            $ H.toHtml
            $ "Inn " ++ show (inning_ logEntry) ++ " " ++ show (halfInning_ logEntry)
        H.div $ case strikeAction_ logEntry of
          NoAction -> H.span ! A.style (stringValue "color: #1a2744;") $ H.toHtml "Ball"
          CalledStrike -> H.span ! A.style (stringValue "color: #8b1a1a;") $ H.toHtml "Strike"
          HitSingle -> H.span ! A.style (stringValue "color: #1a4427;") $ H.toHtml "Single"
          HitDouble -> H.span ! A.style (stringValue "color: #1a4427;") $ H.toHtml "Double"
          HitTriple -> H.span ! A.style (stringValue "color: #1a4427;") $ H.toHtml "Triple"
          HomeRun -> H.span ! A.style (stringValue "color: #c9a227;") $ H.toHtml "Home Run!"
          GroundOut -> H.span ! A.style (stringValue "color: #8b1a1a;") $ H.toHtml "Ground Out"
          FlyOut -> H.span ! A.style (stringValue "color: #8b1a1a;") $ H.toHtml "Fly Out"
          PopOut -> H.span ! A.style (stringValue "color: #8b1a1a;") $ H.toHtml "Pop Out"
          FieldingError -> H.span ! A.style (stringValue "color: #c9a227;") $ H.toHtml "Error"
          HitByPitch -> H.span ! A.style (stringValue "color: #1a2744;") $ H.toHtml "HBP"
      H.div ! A.style (stringValue "margin-top: 3px; font-size: 0.7rem; color: #8b1a1a;") $
        H.toHtml $
          "Score: "
            ++ show (awayScore_ logEntry)
            ++ "-"
            ++ show (homeScore_ logEntry)
            ++ " | "
            ++ show (balls_ logEntry)
            ++ "-"
            ++ show (strikes_ logEntry)
            ++ " | Outs: "
            ++ show (outs_ logEntry)

-- Legacy functions kept for compatibility
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
        ! A.style (stringValue "display: block; margin: 0 auto 20px auto; padding: 10px 20px; background: #1a2744; color: #c9a227; border: 2px solid #c9a227; cursor: pointer; font-family: 'Arial Black', sans-serif; text-transform: uppercase; letter-spacing: 1px;")
        $ H.toHtml "Continue Game"
    else H.div ! A.style (stringValue "text-align: center; margin: 20px;") $ do
      H.p $ H.toHtml "Game Complete!"
      H.button
        ! A.onclick (stringValue "window.location.href='/user'")
        ! A.class_ (stringValue "btn-primary")
        ! A.style (stringValue "margin: 10px;")
        $ H.toHtml "Return to Dashboard"
  gameFrameHtml gs

gameRedirectHtml :: Html
gameRedirectHtml = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml "Starting Game..."
    H.meta ! A.httpEquiv (stringValue "refresh") ! A.content (stringValue "0;url=/data")
  H.body $
    H.p $
      H.toHtml "Starting game..."
