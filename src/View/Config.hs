module View.Config where

import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx as Htmx
import Text.Read (readMaybe)
import WaxBall.Game (GameState (..), Player (..))

configPageToHtml :: [Player] -> [Player] -> Html
configPageToHtml homeTeamPlayers awayTeamPlayers = do
  H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toHtml "Baseball Dice Game - Team Configuration"
      H.script ! A.src (stringValue "https://unpkg.com/htmx.org@1.9.10") $ H.toHtml ""
      H.style $ H.toHtml configPageCSS
    H.body $ do
      H.div ! A.class_ (stringValue "config-container") $ do
        H.h1 $ H.toHtml "Team Configuration"
        H.div ! A.class_ (stringValue "teams-container") $ do
          H.div ! A.class_ (stringValue "team-section") $ do
            H.h2 $ H.toHtml "Home Team"
            mapM_ (renderPlayerForm "home") (zip [0 ..] homeTeamPlayers)
          H.div ! A.class_ (stringValue "team-section") $ do
            H.h2 $ H.toHtml "Away Team"
            mapM_ (renderPlayerForm "away") (zip [0 ..] awayTeamPlayers)
        H.div ! A.class_ (stringValue "start-button-container") $ do
          H.form ! A.action (stringValue "/start-game") ! A.method (stringValue "post") $
            H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "start-game-btn") $
              H.toHtml "Start Game"

renderPlayerForm :: String -> (Int, Player) -> Html
renderPlayerForm teamType (idx, player) = do
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
            ! A.min (stringValue "0.150")
            ! A.max (stringValue "0.400")
            ! A.step (stringValue "0.001")

        H.div ! A.class_ (stringValue "form-row") $ do
          H.label $ H.toHtml "Slugging Percentage: "
          H.input
            ! A.type_ (stringValue "number")
            ! A.name (stringValue "sluggingPercentage")
            ! A.value (stringValue $ show $ sluggingPercentage player)
            ! A.min (stringValue "0.300")
            ! A.max (stringValue "0.700")
            ! A.step (stringValue "0.001")

        H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "update-btn") $ H.toHtml "Update"

updatePlayerFromForm :: GameState -> [(String, String)] -> GameState
updatePlayerFromForm gameState formData =
  let getFormValue key = lookup key formData
      teamType = getFormValue "team"
      playerIndex = getFormValue "player" >>= readMaybe
      newName = getFormValue "name"
      newNumber = getFormValue "number" >>= readMaybe
      newBattingAvg = getFormValue "battingAverage" >>= readMaybe
      newSlugging = getFormValue "sluggingPercentage" >>= readMaybe
   in case (teamType, playerIndex) of
        (Just "home", Just idx) ->
          let updatedHomeTeam = updatePlayerAtIndex (homeBatting gameState) idx newName newNumber newBattingAvg newSlugging
           in gameState {homeBatting = updatedHomeTeam}
        (Just "away", Just idx) ->
          let updatedAwayTeam = updatePlayerAtIndex (awayBatting gameState) idx newName newNumber newBattingAvg newSlugging
           in gameState {awayBatting = updatedAwayTeam}
        _ -> gameState

updatePlayerAtIndex :: [Player] -> Int -> Maybe String -> Maybe Int -> Maybe Double -> Maybe Double -> [Player]
updatePlayerAtIndex players idx mName mNumber mBattingAvg mSlugging =
  let updatePlayer player =
        player
          { name = Data.Maybe.fromMaybe (WaxBall.Game.name player) mName,
            number = fromMaybe (WaxBall.Game.number player) mNumber,
            battingAverage = maybe (battingAverage player) (Prelude.max 0.150 . Prelude.min 0.400) mBattingAvg,
            sluggingPercentage = maybe (sluggingPercentage player) (Prelude.max 0.300 . Prelude.min 0.700) mSlugging
          }
   in case List.splitAt idx players of
        (before, player : after) -> before ++ [updatePlayer player] ++ after
        _ -> players

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
