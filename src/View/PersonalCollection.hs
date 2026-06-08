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
      H.div ! A.class_ (stringValue "pc-stats-summary") $ do
        H.div ! A.class_ (stringValue "panel pc-stat-panel") $ do
          H.div ! A.class_ (stringValue "pc-stat-panel-label") $
            H.toHtml "Total Cards"
          H.div ! A.class_ (stringValue "pc-stat-panel-value") $
            H.toHtml $
              show $
                length $
                  personalCollection user
        H.div ! A.class_ (stringValue "panel pc-stat-panel") $ do
          H.div ! A.class_ (stringValue "pc-stat-panel-label") $
            H.toHtml "Collector"
          H.div ! A.class_ (stringValue "pc-stat-panel-value-sm") $
            H.toHtml $
              AU.name user

      -- Cards grid
      if null (personalCollection user)
        then H.div ! A.class_ (stringValue "panel") $
          H.div ! A.class_ (stringValue "empty-state") $ do
            H.p $ H.toHtml "◆ No Cards Yet ◆"
            H.p
              ! A.class_ (stringValue "pc-empty-hint")
              $ H.toHtml "Start a season to earn cards"
        else
          H.div ! A.class_ (stringValue "pc-cards-grid") $
            mapM_ renderCard (personalCollection user)

renderCard :: Card -> Html
renderCard card =
  H.div ! A.class_ (stringValue "pc-card-flip") $
    H.div ! A.class_ (stringValue "pc-card-flipper") $ do
      -- Front face: the printed card artwork (face-up by default).
      -- Names/numbers will be overlaid on top of the art in a later pass.
      H.div ! A.class_ (stringValue "pc-card-face pc-card-front") $
        H.img
          ! A.class_ (stringValue "pc-card-front-art")
          ! A.src (stringValue "/static/img/card-front.jpg")
          ! A.alt (stringValue "Baseball card")

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
