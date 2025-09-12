module View.User where

import Control.Monad (unless)
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import User.Account (User (..))
import WaxBall.Season (GameResult (..), SeasonState (..), TeamStats (..))

userPageToHtml :: User -> SeasonState -> Html
userPageToHtml user seasonState = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset (stringValue "UTF-8")
    H.meta ! A.name (stringValue "viewport") ! A.content (stringValue "width=device-width, initial-scale=1.0")
    H.title $ H.toHtml "User Dashboard"
    H.script ! A.src (stringValue "https://unpkg.com/htmx.org@1.9.10") $ H.toHtml ""
    H.style $ H.toHtml userPageCSS
  H.body $ do
    H.div ! A.class_ (stringValue "user-container") $ do
      H.h1 $ H.toHtml "User Dashboard"

      -- User Info Section
      H.div ! A.class_ (stringValue "user-info") $ do
        H.h2 $ H.toHtml "User Information"
        H.div ! A.class_ (stringValue "info-grid") $ do
          H.div ! A.class_ (stringValue "info-item") $ do
            H.strong $ H.toHtml "Name: "
            H.span $ H.toHtml $ userName user
          H.div ! A.class_ (stringValue "info-item") $ do
            H.strong $ H.toHtml "Email: "
            H.span $ H.toHtml $ userEmail user
          H.div ! A.class_ (stringValue "info-item") $ do
            H.strong $ H.toHtml "User ID: "
            H.span $ H.toHtml $ show $ userId user
          H.div ! A.class_ (stringValue "info-item") $ do
            H.strong $ H.toHtml "Cards: "
            H.span $ H.toHtml $ show $ length $ cards user

      -- Season Information Section
      H.div ! A.class_ (stringValue "season-info") $ do
        H.h2 $ H.toHtml "Season Information"
        renderSeasonInfo seasonState

      -- New Game Button
      H.div ! A.class_ (stringValue "game-actions") $ do
        if currentGameNumber seasonState > 10
          then do
            H.h3 ! A.style (stringValue "color: #27ae60;") $ H.toHtml "Season Complete!"
            let homeWins = wins (homeTeamStats seasonState)
                awayWins = wins (awayTeamStats seasonState)
                champion
                  | homeWins > awayWins = "Home"
                  | awayWins > homeWins = "Away"
                  | otherwise = "Tie"
            H.p $ H.toHtml $ "Champion: " ++ champion ++ " Team"
            H.form ! A.action (stringValue "/start-season") ! A.method (stringValue "post") $
              H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "new-season-btn") $
                H.toHtml "Start New Season"
          else
            if currentGameNumber seasonState == 1 && null (gameResults seasonState)
              then do
                H.h3 $ H.toHtml "Ready to Start Season?"
                H.form ! A.action (stringValue "/start-season") ! A.method (stringValue "post") $
                  H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "start-game-btn") $
                    H.toHtml "Start New Season"
              else do
                H.h3 $ H.toHtml $ "Ready for Game " ++ show (currentGameNumber seasonState) ++ "?"
                H.form ! A.action (stringValue "/next-game") ! A.method (stringValue "post") $
                  H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "start-game-btn") $
                    H.toHtml $
                      "Start Game " ++ show (currentGameNumber seasonState)

renderSeasonInfo :: SeasonState -> Html
renderSeasonInfo seasonState = do
  H.div ! A.class_ (stringValue "season-progress") $ do
    let gamesPlayed = currentGameNumber seasonState - 1
        totalGames = 10
    H.h3 $ H.toHtml $ "Season Progress: " ++ show gamesPlayed ++ "/" ++ show totalGames ++ " Games"

    -- Progress bar
    let progressPercent = if totalGames > 0 then (fromIntegral gamesPlayed / fromIntegral totalGames) * 100 else 0
    H.div ! A.class_ (stringValue "progress-bar") $ do
      H.div
        ! A.class_ (stringValue "progress-fill")
        ! A.style (stringValue $ "width: " ++ show (progressPercent :: Double) ++ "%;")
        $ H.toHtml ""

  -- Team standings
  H.div ! A.class_ (stringValue "standings") $ do
    H.h3 $ H.toHtml "Team Standings"
    H.table ! A.class_ (stringValue "standings-table") $ do
      H.thead $ H.tr $ do
        H.th $ H.toHtml "Team"
        H.th $ H.toHtml "Wins"
        H.th $ H.toHtml "Losses"
        H.th $ H.toHtml "Win %"
        H.th $ H.toHtml "Runs"
      H.tbody $ do
        let homeStats = homeTeamStats seasonState
            awayStats = awayTeamStats seasonState
            homeWinPct =
              if (wins homeStats + losses homeStats) > 0
                then fromIntegral (wins homeStats) / fromIntegral (wins homeStats + losses homeStats) * 100
                else 0.0
            awayWinPct =
              if (wins awayStats + losses awayStats) > 0
                then fromIntegral (wins awayStats) / fromIntegral (wins awayStats + losses awayStats) * 100
                else 0.0
        H.tr $ do
          H.td $ H.toHtml "Home"
          H.td $ H.toHtml $ show $ wins homeStats
          H.td $ H.toHtml $ show $ losses homeStats
          H.td $ H.toHtml $ printf "%.1f%%" (homeWinPct :: Double)
          H.td $ H.toHtml $ show $ totalRuns homeStats
        H.tr $ do
          H.td $ H.toHtml "Away"
          H.td $ H.toHtml $ show $ wins awayStats
          H.td $ H.toHtml $ show $ losses awayStats
          H.td $ H.toHtml $ printf "%.1f%%" (awayWinPct :: Double)
          H.td $ H.toHtml $ show $ totalRuns awayStats

  -- Recent games (if any)
  unless (null (gameResults seasonState)) $ do
    H.div ! A.class_ (stringValue "recent-games") $ do
      H.h3 $ H.toHtml "Recent Games"
      H.div ! A.class_ (stringValue "games-list") $
        mapM_ renderGameSummary (take 3 $ reverse $ gameResults seasonState)

renderGameSummary :: GameResult -> Html
renderGameSummary result =
  H.div ! A.class_ (stringValue "game-summary") $ do
    H.div ! A.class_ (stringValue "game-title") $ do
      H.strong $ H.toHtml $ "Game " ++ show (gameNumber result)
      H.span ! A.class_ (stringValue "winner") $
        H.toHtml $
          " - " ++ show (winningTeam result) ++ " Wins"
    H.div ! A.class_ (stringValue "game-details") $
      H.toHtml $
        "Score: " ++ show (awayTeamScore result) ++ "-" ++ show (homeTeamScore result)

-- Printf helper for formatting percentages
printf :: String -> Double -> String
printf fmt val = formatDouble fmt val
  where
    formatDouble "%.1f%%" d = show (round (d * 10) `Prelude.div` 10) ++ "." ++ show (round (d * 10) `Prelude.mod` 10) ++ "%"
    formatDouble _ d = show d

userPageCSS :: String
userPageCSS =
  unlines
    [ "body { font-family: Arial, sans-serif; margin: 0; padding: 20px; background: #f5f5f5; }",
      ".user-container { max-width: 1000px; margin: 0 auto; }",
      "h1 { text-align: center; color: #2c3e50; margin-bottom: 40px; }",
      ".user-info, .season-info, .game-actions { background: white; padding: 20px; margin-bottom: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }",
      "h2, h3 { color: #3498db; border-bottom: 2px solid #3498db; padding-bottom: 10px; }",
      ".info-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 15px; }",
      ".info-item { padding: 10px; background: #f8f9fa; border-radius: 5px; }",
      ".season-progress { margin-bottom: 20px; }",
      ".progress-bar { width: 100%; height: 20px; background: #ecf0f1; border-radius: 10px; overflow: hidden; }",
      ".progress-fill { height: 100%; background: linear-gradient(90deg, #3498db, #2980b9); transition: width 0.3s ease; }",
      ".standings { margin: 20px 0; }",
      ".standings-table { width: 100%; border-collapse: collapse; }",
      ".standings-table th, .standings-table td { padding: 10px; text-align: left; border-bottom: 1px solid #ddd; }",
      ".standings-table th { background: #3498db; color: white; }",
      ".standings-table tr:nth-child(even) { background: #f8f9fa; }",
      ".recent-games { margin-top: 20px; }",
      ".games-list { max-height: 200px; overflow-y: auto; }",
      ".game-summary { padding: 10px; margin: 5px 0; background: #f8f9fa; border-left: 4px solid #3498db; border-radius: 5px; }",
      ".game-title { font-size: 1.1em; margin-bottom: 5px; }",
      ".winner { color: #27ae60; }",
      ".game-details { color: #7f8c8d; font-size: 0.9em; }",
      ".game-actions { text-align: center; }",
      ".start-game-btn, .new-season-btn { background: #e74c3c; color: white; border: none; padding: 15px 30px; font-size: 18px; border-radius: 5px; cursor: pointer; margin: 10px; }",
      ".start-game-btn:hover, .new-season-btn:hover { background: #c0392b; }",
      ".new-season-btn { background: #27ae60; }",
      ".new-season-btn:hover { background: #229954; }"
    ]
