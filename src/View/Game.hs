module View.Game where

import Data.Maybe (isJust)
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx as Htmx
import Text.Blaze.Internal (customAttribute, stringTag)
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

-- Render a data-* attribute, e.g. dataAttr "behavior" "redirect" -> data-behavior="redirect".
-- Used to declaratively wire up the TypeScript behavior islands (see ts/main.ts)
-- instead of embedding <script> tags in the HTML DSL.
dataAttr :: String -> String -> H.Attribute
dataAttr key val = customAttribute (stringTag ("data-" ++ key)) (stringValue val)

-- Declarative client-side redirect island: navigates to `url` after `delayMs`.
redirectAfter :: String -> Int -> Html
redirectAfter url delayMs =
  H.div
    ! dataAttr "behavior" "redirect"
    ! dataAttr "redirect-url" url
    ! dataAttr "redirect-delay" (show delayMs)
    $ mempty

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
      H.div ! A.class_ (stringValue "game-status") $ do
        H.p
          ! A.class_ (stringValue "game-status-advancing")
          $ H.toHtml "Auto-advancing..."
        H.div
          ! Htmx.hxGet (stringValue "/game-data")
          ! Htmx.hxTarget (stringValue "#game-frame")
          ! Htmx.hxSwap (stringValue "outerHTML")
          ! Htmx.hxTrigger (stringValue "every 0.5s")
          $ H.toHtml ""
    else do
      H.div ! A.class_ (stringValue "game-status") $ do
        H.p
          ! A.class_ (stringValue "game-status-complete")
          $ H.toHtml "Game Complete! Redirecting..."
        redirectAfter "/user" 1000
  gameFrameHtml gs

autoAdvancingGameFrameHtml :: GameState -> Html
autoAdvancingGameFrameHtml = gameFrameHtml

gameCompletionHtml :: GameState -> Html
gameCompletionHtml gs =
  H.div ! A.id (stringValue "game-frame") ! A.class_ (stringValue "game-frame") $ do
    H.div
      ! A.class_ (stringValue "panel completion-panel")
      $ do
        H.h2
          ! A.class_ (stringValue "completion-title")
          $ H.toHtml "Game Complete!"
        let finalHomeScore = homeScore gs
            finalAwayScore = awayScore gs
            winner = if finalHomeScore > finalAwayScore then "Home" else "Away"
        H.div
          ! A.class_ (stringValue "completion-winner")
          $ H.toHtml
          $ winner ++ " Team Wins!"
        H.div
          ! A.class_ (stringValue "completion-score")
          $ H.toHtml
          $ "Final: Away " ++ show finalAwayScore ++ " — Home " ++ show finalHomeScore
        H.div
          ! A.class_ (stringValue "completion-innings")
          $ H.toHtml
          $ "Innings: " ++ show (inning gs)
        H.p
          ! A.class_ (stringValue "completion-note")
          $ H.toHtml "Returning to dashboard..."
        redirectAfter "/user" 2000

gameFrameHtml :: GameState -> Html
gameFrameHtml gs = H.div ! A.id (stringValue "game-frame") ! A.class_ (stringValue "game-frame") $ do
  H.div ! A.class_ (stringValue "game-flex") $ do
    H.div ! A.class_ (stringValue "game-main") $ do
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
    H.div ! A.class_ (stringValue "game-log game-side") $ do
      H.h3
        ! A.class_ (stringValue "game-log-title")
        $ H.toHtml "Game Log"
      if null (pitchLog gs)
        then
          H.p
            ! A.class_ (stringValue "game-log-empty")
            $ H.toHtml "No pitches yet..."
        else
          H.div ! A.class_ (stringValue "log-entries") $
            mapM_ renderLogEntry (pitchLog gs)

renderLogEntry :: Log -> Html
renderLogEntry logEntry =
  H.div
    ! A.class_ (stringValue "log-entry")
    $ do
      H.div ! A.class_ (stringValue "log-entry-head") $ do
        H.div ! A.class_ (stringValue "log-entry-batter") $ do
          H.toHtml $
            maybe "Unknown" (\p -> WaxBall.Game.name p) (currentBatter_ logEntry)
          H.span
            ! A.class_ (stringValue "log-entry-inning")
            $ H.toHtml
            $ "Inn " ++ show (inning_ logEntry) ++ " " ++ show (halfInning_ logEntry)
        H.div $ case strikeAction_ logEntry of
          NoAction -> H.span ! A.class_ (stringValue "log-neutral") $ H.toHtml "Ball"
          CalledStrike -> H.span ! A.class_ (stringValue "log-bad") $ H.toHtml "Strike"
          HitSingle -> H.span ! A.class_ (stringValue "log-good") $ H.toHtml "Single"
          HitDouble -> H.span ! A.class_ (stringValue "log-good") $ H.toHtml "Double"
          HitTriple -> H.span ! A.class_ (stringValue "log-good") $ H.toHtml "Triple"
          HomeRun -> H.span ! A.class_ (stringValue "log-great") $ H.toHtml "Home Run!"
          GroundOut -> H.span ! A.class_ (stringValue "log-bad") $ H.toHtml "Ground Out"
          FlyOut -> H.span ! A.class_ (stringValue "log-bad") $ H.toHtml "Fly Out"
          PopOut -> H.span ! A.class_ (stringValue "log-bad") $ H.toHtml "Pop Out"
          FieldingError -> H.span ! A.class_ (stringValue "log-great") $ H.toHtml "Error"
          HitByPitch -> H.span ! A.class_ (stringValue "log-neutral") $ H.toHtml "HBP"
      H.div ! A.class_ (stringValue "log-entry-score") $
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
