module View.Season where

import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx as Htmx
import User.AuthenticatedUser (AuthenticatedUser)
import View.Layout (mainLayout)
import WaxBall.Game (Player (..))
import WaxBall.Season (GameResult (..), SeasonState (..), TeamStats (..))

seasonPageToHtml :: AuthenticatedUser -> SeasonState -> Html
seasonPageToHtml user seasonState =
  mainLayout user "Season" $
    H.div $ do
      H.h1 ! A.class_ (stringValue "page-title") $ H.toHtml "Season"
      seasonContentHtml seasonState

seasonConfigPageToHtml :: AuthenticatedUser -> [Player] -> [Player] -> Html
seasonConfigPageToHtml user homeTeamPlayers awayTeamPlayers =
  mainLayout user "Season Config" $
    H.div $ do
      H.h1 ! A.class_ (stringValue "page-title") $ H.toHtml "Team Configuration"
      H.div ! A.style (stringValue "display: flex; gap: 32px; flex-wrap: wrap;") $ do
        H.div ! A.style (stringValue "flex: 1; min-width: 300px;") $ do
          H.div ! A.class_ (stringValue "panel") $ do
            H.div ! A.class_ (stringValue "panel-title") $ H.toHtml "Home Team"
            mapM_ (renderSeasonPlayerForm "home") (zip [0 ..] homeTeamPlayers)
        H.div ! A.style (stringValue "flex: 1; min-width: 300px;") $ do
          H.div ! A.class_ (stringValue "panel") $ do
            H.div ! A.class_ (stringValue "panel-title") $ H.toHtml "Away Team"
            mapM_ (renderSeasonPlayerForm "away") (zip [0 ..] awayTeamPlayers)
      H.div ! A.style (stringValue "text-align: center; margin-top: 32px;") $ do
        H.form ! A.action (stringValue "/start-game") ! A.method (stringValue "post") $
          H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "btn-danger") $
            H.toHtml "Start Game"

-- HTMX fragment for /game-frame when no game is active
gameFrameSeasonFragment :: SeasonState -> Html
gameFrameSeasonFragment seasonState = do
  H.div ! A.class_ (stringValue "panel-title") $ H.toHtml "Season"
  seasonContentHtml seasonState

seasonContentHtml :: SeasonState -> Html
seasonContentHtml seasonState = do
  if currentGameNumber seasonState == 1 && null (gameResults seasonState)
    then do
      H.div ! A.class_ (stringValue "panel") $ do
        H.div ! A.class_ (stringValue "panel-title") $ H.toHtml "Ready to Play?"
        H.p ! A.style (stringValue "margin-bottom: 16px; color: #8b1a1a;") $
          H.toHtml "Start a new 10-game season to begin."
        H.form ! A.action (stringValue "/start-season") ! A.method (stringValue "post") $
          H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "btn-primary") $
            H.toHtml "Start New Season"
    else do
      -- Progress
      H.div ! A.class_ (stringValue "panel") $ do
        H.div ! A.class_ (stringValue "panel-title") $
          H.toHtml $
            "Season Progress: Game " ++ show (currentGameNumber seasonState - 1) ++ " / 10"
        let gamesPlayed = currentGameNumber seasonState - 1
            progressPct = (fromIntegral gamesPlayed / 10.0) * 100 :: Double
        H.div ! A.class_ (stringValue "progress-bar")
          $ H.div
            ! A.class_ (stringValue "progress-fill")
            ! A.style (stringValue $ "width: " ++ show progressPct ++ "%;")
          $ H.toHtml ""

      -- Standings
      H.div ! A.class_ (stringValue "panel") $ do
        H.div ! A.class_ (stringValue "panel-title") $ H.toHtml "Standings"
        H.table ! A.class_ (stringValue "data-table") $ do
          H.thead $ H.tr $ do
            H.th $ H.toHtml "Team"
            H.th $ H.toHtml "W"
            H.th $ H.toHtml "L"
            H.th $ H.toHtml "R"
            H.th $ H.toHtml "RA"
          H.tbody $ do
            H.tr $ do
              H.td $ H.toHtml "Home"
              H.td $ H.toHtml $ show $ wins (homeTeamStats seasonState)
              H.td $ H.toHtml $ show $ losses (homeTeamStats seasonState)
              H.td $ H.toHtml $ show $ totalRuns (homeTeamStats seasonState)
              H.td $ H.toHtml $ show $ totalRunsAllowed (homeTeamStats seasonState)
            H.tr $ do
              H.td $ H.toHtml "Away"
              H.td $ H.toHtml $ show $ wins (awayTeamStats seasonState)
              H.td $ H.toHtml $ show $ losses (awayTeamStats seasonState)
              H.td $ H.toHtml $ show $ totalRuns (awayTeamStats seasonState)
              H.td $ H.toHtml $ show $ totalRunsAllowed (awayTeamStats seasonState)

      -- Recent games
      unless (null (gameResults seasonState)) $
        H.div ! A.class_ (stringValue "panel") $ do
          H.div ! A.class_ (stringValue "panel-title") $ H.toHtml "Recent Games"
          mapM_ renderGameResult (take 5 $ reverse $ gameResults seasonState)

      -- Next game action
      if currentGameNumber seasonState <= 10
        then
          H.div ! A.style (stringValue "margin-top: 16px;") $
            H.form ! A.action (stringValue "/next-game") ! A.method (stringValue "post") $
              H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "btn-primary") $
                H.toHtml $
                  "Configure Game " ++ show (currentGameNumber seasonState)
        else H.div ! A.class_ (stringValue "panel") $ do
          H.div ! A.class_ (stringValue "panel-title") $ H.toHtml "Season Complete!"
          let homeWins = wins (homeTeamStats seasonState)
              awayWins = wins (awayTeamStats seasonState)
              champion
                | homeWins > awayWins = "Home"
                | awayWins > homeWins = "Away"
                | otherwise = "Tie"
          H.p ! A.style (stringValue "margin-bottom: 16px;") $
            H.toHtml $
              "Champion: " ++ champion ++ " Team"
          H.form ! A.action (stringValue "/start-season") ! A.method (stringValue "post") $
            H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "btn-primary") $
              H.toHtml "Start New Season"

renderSeasonPlayerForm :: String -> (Int, Player) -> Html
renderSeasonPlayerForm teamType (idx, player) = do
  H.div
    ! A.class_ (stringValue "player-form")
    ! A.id (stringValue $ "player-" ++ teamType ++ "-" ++ show idx)
    $ do
      H.h3 $
        H.toHtml $
          WaxBall.Game.name player ++ " (#" ++ show (WaxBall.Game.number player) ++ ")"
      H.form
        ! Htmx.hxPost (stringValue "/update-player")
        ! Htmx.hxTarget (stringValue $ "#player-" ++ teamType ++ "-" ++ show idx)
        ! Htmx.hxSwap (stringValue "outerHTML")
        $ do
          H.input ! A.type_ (stringValue "hidden") ! A.name (stringValue "team") ! A.value (stringValue teamType)
          H.input ! A.type_ (stringValue "hidden") ! A.name (stringValue "player") ! A.value (stringValue $ show idx)

          H.div ! A.class_ (stringValue "config-form-row") $ do
            H.label $ H.toHtml "Name"
            H.input
              ! A.type_ (stringValue "text")
              ! A.name (stringValue "name")
              ! A.value (stringValue $ WaxBall.Game.name player)

          H.div ! A.class_ (stringValue "config-form-row") $ do
            H.label $ H.toHtml "Number"
            H.input
              ! A.type_ (stringValue "number")
              ! A.name (stringValue "number")
              ! A.value (stringValue $ show $ WaxBall.Game.number player)
              ! A.min (stringValue "1")
              ! A.max (stringValue "99")

          H.div ! A.class_ (stringValue "config-form-row") $ do
            H.label $ H.toHtml "Batting Avg"
            H.input
              ! A.type_ (stringValue "number")
              ! A.name (stringValue "battingAverage")
              ! A.value (stringValue $ show $ battingAverage player)
              ! A.step (stringValue "0.001")

          H.div ! A.class_ (stringValue "config-form-row") $ do
            H.label $ H.toHtml "Slugging %"
            H.input
              ! A.type_ (stringValue "number")
              ! A.name (stringValue "sluggingPercentage")
              ! A.value (stringValue $ show $ sluggingPercentage player)
              ! A.step (stringValue "0.001")

          H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "update-btn") $
            H.toHtml "Update"

renderGameResult :: GameResult -> Html
renderGameResult result =
  H.div ! A.class_ (stringValue "game-result-row") $ do
    H.span ! A.class_ (stringValue "winner") $
      H.toHtml $
        "Game " ++ show (gameNumber result) ++ " — " ++ show (winningTeam result) ++ " wins"
    H.span ! A.style (stringValue "margin-left: 16px; color: #1a2744; font-size: 0.85rem;") $
      H.toHtml $
        show (awayTeamScore result) ++ "–" ++ show (homeTeamScore result)

unless :: Bool -> Html -> Html
unless condition htmlContent = if condition then H.toHtml "" else htmlContent
