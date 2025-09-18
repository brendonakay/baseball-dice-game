module View.PersonalCollection where

import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import User.AuthenticatedUser as AU
import WaxBall.Card as C
import WaxBall.Game as G

personalCollectionPageToHtml :: AuthenticatedUser -> Html
personalCollectionPageToHtml user = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset (stringValue "UTF-8")
    H.meta ! A.name (stringValue "viewport") ! A.content (stringValue "width=device-width, initial-scale=1.0")
    H.title $ H.toHtml "Personal Collection"
    H.script ! A.src (stringValue "https://unpkg.com/htmx.org@1.9.10") $ H.toHtml ""
    H.style $ H.toHtml personalCollectionCSS
  H.body $ do
    H.div ! A.class_ (stringValue "collection-container") $ do
      H.div ! A.class_ (stringValue "header") $ do
        H.h1 $ H.toHtml "Personal Collection"
        H.a ! A.href (stringValue "/user") ! A.class_ (stringValue "back-btn") $ H.toHtml "<- Back to Dashboard"

      H.div ! A.class_ (stringValue "collection-stats") $ do
        H.div ! A.class_ (stringValue "stat-card") $ do
          H.h3 $ H.toHtml "Total Cards"
          H.p ! A.class_ (stringValue "stat-value") $ H.toHtml $ show $ length $ personalCollection user
        H.div ! A.class_ (stringValue "stat-card") $ do
          H.h3 $ H.toHtml "Collection Owner"
          H.p ! A.class_ (stringValue "stat-value") $ H.toHtml $ AU.name user

      H.div ! A.class_ (stringValue "cards-grid") $ do
        mapM_ renderCard (personalCollection user)

renderCard :: Card -> Html
renderCard card = H.div ! A.class_ (stringValue "card") $ do
  H.div ! A.class_ (stringValue "card-header") $ do
    H.span ! A.class_ (stringValue "card-number") $ H.toHtml $ C.number card
    H.span ! A.class_ (stringValue "jersey-number") $ H.toHtml $ "#" ++ show (G.number (player card))
  H.div ! A.class_ (stringValue "card-body") $ do
    H.h3 ! A.class_ (stringValue "player-name") $ H.toHtml $ G.name (player card)
    H.div ! A.class_ (stringValue "stats") $ do
      H.div ! A.class_ (stringValue "stat") $ do
        H.span ! A.class_ (stringValue "stat-label") $ H.toHtml "AVG"
        H.span ! A.class_ (stringValue "stat-value") $ H.toHtml $ formatAverage $ battingAverage (player card)
      H.div ! A.class_ (stringValue "stat") $ do
        H.span ! A.class_ (stringValue "stat-label") $ H.toHtml "OBP"
        H.span ! A.class_ (stringValue "stat-value") $ H.toHtml $ formatAverage $ onBasePercentage (player card)
      H.div ! A.class_ (stringValue "stat") $ do
        H.span ! A.class_ (stringValue "stat-label") $ H.toHtml "SLG"
        H.span ! A.class_ (stringValue "stat-value") $ H.toHtml $ formatAverage $ sluggingPercentage (player card)

formatAverage :: Double -> String
formatAverage avg =
  let formatted = show (round (avg * 1000) :: Int)
   in case length formatted of
        1 -> ".00" ++ formatted
        2 -> ".0" ++ formatted
        _ -> "." ++ formatted

personalCollectionCSS :: String
personalCollectionCSS =
  unlines
    [ "body { font-family: 'Helvetica Neue', Arial, sans-serif; margin: 0; padding: 0; background: #f5f5f5; }",
      ".collection-container { max-width: 1200px; margin: 0 auto; padding: 20px; }",
      ".header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 30px; }",
      ".header h1 { margin: 0; color: #2c3e50; font-family: 'Arial Black', sans-serif; text-transform: uppercase; letter-spacing: 2px; }",
      ".back-btn { text-decoration: none; color: #3498db; padding: 10px 20px; border: 2px solid #3498db; background: transparent; transition: all 0.3s; font-weight: bold; }",
      ".back-btn:hover { background: #3498db; color: white; }",
      ".collection-stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin-bottom: 30px; }",
      ".stat-card { background: white; padding: 20px; border: 2px solid #ddd; box-shadow: 0 4px 8px rgba(0,0,0,0.1); text-align: center; }",
      ".stat-card h3 { margin: 0 0 10px 0; color: #7f8c8d; font-size: 14px; text-transform: uppercase; letter-spacing: 1px; }",
      ".stat-card .stat-value { margin: 0; font-size: 28px; font-weight: bold; color: #2c3e50; }",
      ".cards-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap: 25px; }",
      ".card { background: white; border: 3px solid #2c3e50; box-shadow: 5px 5px 15px rgba(0,0,0,0.2); overflow: hidden; transition: transform 0.2s, box-shadow 0.2s; position: relative; }",
      ".card:hover { transform: translateY(-8px) rotateZ(-1deg); box-shadow: 8px 8px 20px rgba(0,0,0,0.3); }",
      ".card-header { background: linear-gradient(135deg, #3498db, #2980b9); color: white; padding: 12px 15px; display: flex; justify-content: space-between; align-items: center; border-bottom: 2px solid #2c3e50; }",
      ".card-number { font-weight: bold; font-size: 12px; letter-spacing: 1px; text-shadow: 1px 1px 2px rgba(0,0,0,0.3); }",
      ".jersey-number { font-size: 24px; font-weight: bold; font-family: 'Arial Black', sans-serif; text-shadow: 2px 2px 3px rgba(0,0,0,0.3); }",
      ".card-body { padding: 20px; background: linear-gradient(to bottom, #fff 0%, #f8f9fa 100%); }",
      ".player-name { margin: 0 0 20px 0; color: #2c3e50; font-size: 22px; font-weight: bold; text-align: center; font-family: 'Arial Black', sans-serif; text-transform: uppercase; letter-spacing: 1px; }",
      ".stats { display: grid; grid-template-columns: repeat(3, 1fr); gap: 8px; margin-top: 15px; padding-top: 15px; border-top: 2px solid #3498db; }",
      ".stat { text-align: center; padding: 8px; background: #f8f9fa; border: 1px solid #ddd; }",
      ".stat-label { display: block; font-size: 11px; color: #7f8c8d; margin-bottom: 3px; font-weight: bold; text-transform: uppercase; }",
      ".stat-value { display: block; font-size: 20px; font-weight: bold; color: #2c3e50; font-family: 'Courier New', monospace; }"
    ]
