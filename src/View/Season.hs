module View.Season where

import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx as Htmx
import WaxBall.Game (Player (..))
import WaxBall.Season (GameResult (..), SeasonState (..), TeamStats (..))

seasonPageToHtml :: SeasonState -> Html
seasonPageToHtml seasonState = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset (stringValue "UTF-8")
    H.meta ! A.name (stringValue "viewport") ! A.content (stringValue "width=device-width, initial-scale=1.0")
    H.title $ H.toHtml "Baseball Season"
    H.script ! A.src (stringValue "https://unpkg.com/htmx.org@1.9.10") $ H.toHtml ""
    H.style $ H.toHtml seasonPageCSS
  H.body $ do
    H.div ! A.class_ (stringValue "season-container") $ do
      H.h1 $ H.toHtml "Baseball Season"

      if currentGameNumber seasonState == 1 && null (gameResults seasonState)
        then do
          H.div ! A.class_ (stringValue "welcome-section") $ do
            H.h2 $ H.toHtml "Welcome to the Baseball Season!"
            H.p $ H.toHtml "Ready to play a 10-game season?"
            H.form ! A.action (stringValue "/start-season") ! A.method (stringValue "post") $
              H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "start-season-btn") $
                H.toHtml "Start New Season!"
        else do
          H.div ! A.class_ (stringValue "season-stats") $ do
            H.h2 $ H.toHtml $ "Season Progress: Game " ++ show (currentGameNumber seasonState - 1) ++ " of 10"

            H.div ! A.class_ (stringValue "standings") $ do
              H.h3 $ H.toHtml "Team Standings"
              H.table ! A.class_ (stringValue "standings-table") $ do
                H.thead $ H.tr $ do
                  H.th $ H.toHtml "Team"
                  H.th $ H.toHtml "Wins"
                  H.th $ H.toHtml "Losses"
                  H.th $ H.toHtml "Runs Scored"
                  H.th $ H.toHtml "Runs Allowed"
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

          unless (null (gameResults seasonState)) $ do
            H.div ! A.class_ (stringValue "game-results") $ do
              H.h3 $ H.toHtml "Recent Games"
              H.div ! A.class_ (stringValue "results-list") $
                mapM_ renderGameResult (reverse $ gameResults seasonState)

          if currentGameNumber seasonState <= 10
            then do
              H.div ! A.class_ (stringValue "next-game-section") $ do
                H.form ! A.action (stringValue "/next-game") ! A.method (stringValue "post") $
                  H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "next-game-btn") $
                    H.toHtml $
                      "Start Game " ++ show (currentGameNumber seasonState)
            else do
              H.div ! A.class_ (stringValue "season-complete") $ do
                H.h2 ! A.style (stringValue "color: #e74c3c;") $ H.toHtml "Season Complete!"
                let homeWins = wins (homeTeamStats seasonState)
                    awayWins = wins (awayTeamStats seasonState)
                    champion
                      | homeWins > awayWins = "Home"
                      | awayWins > homeWins = "Away"
                      | otherwise = "Tie"
                H.h3 $ H.toHtml $ "Champion: " ++ champion ++ " Team"

seasonConfigPageToHtml :: [Player] -> [Player] -> Html
seasonConfigPageToHtml homeTeamPlayers awayTeamPlayers = do
  H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toHtml "Season Team Configuration"
      H.script ! A.src (stringValue "https://unpkg.com/htmx.org@1.9.10") $ H.toHtml ""
      H.style $ H.toHtml configPageCSS
    H.body $ do
      H.div ! A.class_ (stringValue "config-container") $ do
        H.h1 $ H.toHtml "Season Team Configuration"
        H.div ! A.class_ (stringValue "teams-container") $ do
          H.div ! A.class_ (stringValue "team-section") $ do
            H.h2 $ H.toHtml "Home Team"
            mapM_ (renderSeasonPlayerForm "home") (zip [0 ..] homeTeamPlayers)
          H.div ! A.class_ (stringValue "team-section") $ do
            H.h2 $ H.toHtml "Away Team"
            mapM_ (renderSeasonPlayerForm "away") (zip [0 ..] awayTeamPlayers)
        H.div ! A.class_ (stringValue "start-button-container") $ do
          H.form ! A.action (stringValue "/start-game") ! A.method (stringValue "post") $
            H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "start-game-btn") $
              H.toHtml "Start Game"

renderSeasonPlayerForm :: String -> (Int, Player) -> Html
renderSeasonPlayerForm teamType (idx, player) = do
  H.div ! A.class_ (stringValue "player-form") ! A.id (stringValue $ "player-" ++ teamType ++ "-" ++ show idx) $ do
    H.h3 $ H.toHtml $ WaxBall.Game.name player ++ " (#" ++ show (WaxBall.Game.number player) ++ ")"
    H.form
      ! Htmx.hxPost (stringValue "/update-player")
      ! Htmx.hxTarget (stringValue $ "#player-" ++ teamType ++ "-" ++ show idx)
      ! Htmx.hxSwap (stringValue "outerHTML")
      $ do
        H.input ! A.type_ (stringValue "hidden") ! A.name (stringValue "team") ! A.value (stringValue teamType)
        H.input ! A.type_ (stringValue "hidden") ! A.name (stringValue "player") ! A.value (stringValue $ show idx)

        H.div ! A.class_ (stringValue "form-row") $ do
          H.label $ H.toHtml "Name: "
          H.input ! A.type_ (stringValue "text") ! A.name (stringValue "name") ! A.value (stringValue $ WaxBall.Game.name player)

        H.div ! A.class_ (stringValue "form-row") $ do
          H.label $ H.toHtml "Number: "
          H.input ! A.type_ (stringValue "number") ! A.name (stringValue "number") ! A.value (stringValue $ show $ WaxBall.Game.number player) ! A.min (stringValue "1") ! A.max (stringValue "99")

        H.div ! A.class_ (stringValue "form-row") $ do
          H.label $ H.toHtml "Batting Average: "
          H.input
            ! A.type_ (stringValue "number")
            ! A.name (stringValue "battingAverage")
            ! A.value (stringValue $ show $ battingAverage player)
            ! A.step (stringValue "0.001")

        H.div ! A.class_ (stringValue "form-row") $ do
          H.label $ H.toHtml "Slugging Percentage: "
          H.input
            ! A.type_ (stringValue "number")
            ! A.name (stringValue "sluggingPercentage")
            ! A.value (stringValue $ show $ sluggingPercentage player)
            ! A.step (stringValue "0.001")

        H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "update-btn") $ H.toHtml "Update"

renderGameResult :: GameResult -> Html
renderGameResult result =
  H.div ! A.class_ (stringValue "game-result") $ do
    H.div ! A.class_ (stringValue "game-header") $ do
      H.strong $ H.toHtml $ "Game " ++ show (gameNumber result)
      H.span ! A.class_ (stringValue "winner") $
        H.toHtml $
          " - " ++ show (winningTeam result) ++ " Team Wins!"
    H.div ! A.class_ (stringValue "game-score") $
      H.toHtml $
        "Final Score: " ++ show (awayTeamScore result) ++ "-" ++ show (homeTeamScore result)
    H.div ! A.class_ (stringValue "game-innings") $
      H.toHtml $
        "Innings: " ++ show (totalInnings result)

unless :: Bool -> Html -> Html
unless condition htmlContent = if condition then H.toHtml "" else htmlContent

configPageCSS :: String
configPageCSS =
  unlines
    [ "body { font-family: Arial, sans-serif; margin: 0; padding: 20px; background: #f5f5f5; }",
      ".config-container { max-width: 1200px; margin: 0 auto; }",
      "h1 { text-align: center; color: #2c3e50; margin-bottom: 40px; }",
      ".teams-container { display: flex; gap: 40px; }",
      ".team-section { flex: 1; background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }",
      "h2 { color: #3498db; border-bottom: 2px solid #3498db; padding-bottom: 10px; }",
      ".player-form { background: #f8f9fa; padding: 15px; margin: 15px 0; border-radius: 5px; border-left: 4px solid #3498db; }",
      "h3 { margin: 0 0 15px 0; color: #2c3e50; }",
      ".form-row { margin: 10px 0; display: flex; align-items: center; }",
      "label { min-width: 150px; font-weight: bold; }",
      "input { padding: 5px; border: 1px solid #ddd; border-radius: 3px; flex: 1; margin-left: 10px; }",
      ".update-btn { background: #27ae60; color: white; border: none; padding: 5px 15px; border-radius: 3px; cursor: pointer; margin-top: 10px; }",
      ".update-btn:hover { background: #229954; }",
      ".start-button-container { text-align: center; margin: 40px 0; }",
      ".start-game-btn { background: #e74c3c; color: white; border: none; padding: 15px 40px; font-size: 18px; border-radius: 5px; cursor: pointer; }",
      ".start-game-btn:hover { background: #c0392b; }"
    ]

seasonPageCSS :: String
seasonPageCSS =
  unlines
    [ "body { font-family: Arial, sans-serif; margin: 0; padding: 20px; background: #f5f5f5; }",
      ".season-container { max-width: 1000px; margin: 0 auto; }",
      "h1 { text-align: center; color: #2c3e50; margin-bottom: 40px; }",
      ".welcome-section { text-align: center; background: white; padding: 40px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }",
      ".start-season-btn { background: #e74c3c; color: white; border: none; padding: 15px 30px; font-size: 18px; border-radius: 5px; cursor: pointer; }",
      ".start-season-btn:hover { background: #c0392b; }",
      ".season-stats { background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); margin-bottom: 20px; }",
      ".standings { margin: 20px 0; }",
      ".standings-table { width: 100%; border-collapse: collapse; }",
      ".standings-table th, .standings-table td { padding: 10px; text-align: left; border-bottom: 1px solid #ddd; }",
      ".standings-table th { background: #3498db; color: white; }",
      ".game-results { background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); margin-bottom: 20px; }",
      ".results-list { max-height: 400px; overflow-y: auto; }",
      ".game-result { padding: 15px; margin: 10px 0; background: #f8f9fa; border-left: 4px solid #3498db; border-radius: 5px; }",
      ".game-header { font-size: 1.1em; margin-bottom: 5px; }",
      ".winner { color: #27ae60; }",
      ".game-score, .game-innings { color: #7f8c8d; font-size: 0.9em; }",
      ".next-game-section { text-align: center; background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }",
      ".next-game-btn { background: #3498db; color: white; border: none; padding: 15px 30px; font-size: 16px; border-radius: 5px; cursor: pointer; }",
      ".next-game-btn:hover { background: #2980b9; }",
      ".season-complete { text-align: center; background: white; padding: 40px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }"
    ]
