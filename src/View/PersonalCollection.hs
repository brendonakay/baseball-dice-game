module View.PersonalCollection where

import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import User.AuthenticatedUser as AU
import View.Layout (mainLayout)
import WaxBall.Card as C
import WaxBall.Game as G

personalCollectionPageToHtml :: AuthenticatedUser -> Html
personalCollectionPageToHtml user =
  mainLayout user "Personal Collection" $
    H.div $ do
      H.h1 ! A.class_ (stringValue "page-title") $ H.toHtml "Personal Collection"

      -- Stats row
      H.div ! A.style (stringValue "display: flex; gap: 16px; margin-bottom: 24px;") $ do
        H.div ! A.class_ (stringValue "panel") ! A.style (stringValue "flex: 1; text-align: center; padding: 16px;") $ do
          H.div ! A.style (stringValue "font-family: 'Arial Black', sans-serif; font-size: 0.65rem; color: #8b1a1a; text-transform: uppercase; letter-spacing: 2px; margin-bottom: 6px;") $
            H.toHtml "Total Cards"
          H.div ! A.style (stringValue "font-size: 2rem; font-weight: bold; color: #1a2744; font-family: 'Arial Black', sans-serif;") $
            H.toHtml $
              show $
                length $
                  personalCollection user
        H.div ! A.class_ (stringValue "panel") ! A.style (stringValue "flex: 1; text-align: center; padding: 16px;") $ do
          H.div ! A.style (stringValue "font-family: 'Arial Black', sans-serif; font-size: 0.65rem; color: #8b1a1a; text-transform: uppercase; letter-spacing: 2px; margin-bottom: 6px;") $
            H.toHtml "Collector"
          H.div ! A.style (stringValue "font-size: 1.4rem; font-weight: bold; color: #1a2744; font-family: 'Arial Black', sans-serif; text-transform: uppercase; letter-spacing: 2px;") $
            H.toHtml $
              AU.name user

      -- Cards grid
      if null (personalCollection user)
        then H.div ! A.class_ (stringValue "panel") $
          H.div ! A.class_ (stringValue "empty-state") $ do
            H.p $ H.toHtml "◆ No Cards Yet ◆"
            H.p
              ! A.style (stringValue "font-size: 0.75rem; color: #8b1a1a; margin-top: 8px; letter-spacing: 1px;")
              $ H.toHtml "Start a season to earn cards"
        else
          H.div ! A.class_ (stringValue "pc-cards-grid") $
            mapM_ renderCard (personalCollection user)

renderCard :: Card -> Html
renderCard card =
  H.div ! A.class_ (stringValue "pc-card-flip") $
    H.div ! A.class_ (stringValue "pc-card-flipper") $ do
      -- Front face: player photo placeholder (default, face-up)
      H.div ! A.class_ (stringValue "pc-card-face pc-card-front") $ do
        H.div ! A.class_ (stringValue "pc-card-header") $ do
          H.span ! A.class_ (stringValue "pc-set-name") $ H.toHtml $ C.number card
          H.span ! A.class_ (stringValue "pc-jersey") $
            H.toHtml $
              "#" ++ show (G.number (player card))
        H.div ! A.class_ (stringValue "pc-front-photo") $ do
          -- Ghost jersey number behind the photo area
          H.span ! A.class_ (stringValue "pc-front-jersey-bg") $
            H.toHtml $
              show (G.number (player card))
          -- Baseball icon placeholder
          H.span ! A.class_ (stringValue "pc-front-photo-icon") $
            H.toHtml "\9918"
          H.span ! A.class_ (stringValue "pc-front-photo-label") $
            H.toHtml "Player Photo"
        H.div ! A.class_ (stringValue "pc-front-footer") $ do
          H.div ! A.class_ (stringValue "pc-front-name") $
            H.toHtml $
              G.name (player card)
          H.div ! A.class_ (stringValue "pc-front-team-label") $
            H.toHtml $
              team card

      -- Back face: stats (revealed on hover)
      H.div ! A.class_ (stringValue "pc-card-face pc-card-back") $ do
        H.div ! A.class_ (stringValue "pc-card-header") $ do
          H.span ! A.class_ (stringValue "pc-set-name") $ H.toHtml $ C.number card
          H.span ! A.class_ (stringValue "pc-jersey") $
            H.toHtml $
              "#" ++ show (G.number (player card))
        H.div ! A.class_ (stringValue "pc-photo-area") $
          H.toHtml "\9670 PHOTO \9670"
        H.div ! A.class_ (stringValue "pc-card-body") $ do
          H.div ! A.class_ (stringValue "pc-player-name") $
            H.toHtml $
              G.name (player card)
          H.div ! A.class_ (stringValue "pc-team") $
            H.toHtml $
              team card
          H.div ! A.class_ (stringValue "pc-stats-row") $ do
            H.div ! A.class_ (stringValue "pc-stat") $ do
              H.span ! A.class_ (stringValue "pc-stat-label") $ H.toHtml "AVG"
              H.span ! A.class_ (stringValue "pc-stat-value") $
                H.toHtml $
                  formatAverage $
                    battingAverage (player card)
            H.div ! A.class_ (stringValue "pc-stat") $ do
              H.span ! A.class_ (stringValue "pc-stat-label") $ H.toHtml "OBP"
              H.span ! A.class_ (stringValue "pc-stat-value") $
                H.toHtml $
                  formatAverage $
                    onBasePercentage (player card)
            H.div ! A.class_ (stringValue "pc-stat") $ do
              H.span ! A.class_ (stringValue "pc-stat-label") $ H.toHtml "SLG"
              H.span ! A.class_ (stringValue "pc-stat-value") $
                H.toHtml $
                  formatAverage $
                    sluggingPercentage (player card)
        H.div ! A.class_ (stringValue "pc-type-badge") $
          H.toHtml $
            cardTypeLabel (cardType card)

cardTypeLabel :: Type -> String
cardTypeLabel (Type Base Nothing) = "Base"
cardTypeLabel (Type Parallel Nothing) = "Parallel"
cardTypeLabel (Type Insert Nothing) = "Insert"
cardTypeLabel (Type _ (Just Autograph)) = "Auto"
cardTypeLabel (Type _ (Just Serial)) = "Serial"

formatAverage :: Double -> String
formatAverage avg =
  let formatted = show (round (avg * 1000) :: Int)
   in case length formatted of
        1 -> ".00" ++ formatted
        2 -> ".0" ++ formatted
        _ -> "." ++ formatted
