module View.User where

import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx as Htmx
import User.AuthenticatedUser (AuthenticatedUser (..))
import View.Layout (mainLayout)
import WaxBall.Season (SeasonState)

userPageToHtml :: AuthenticatedUser -> SeasonState -> Html
userPageToHtml user _seasonState =
  mainLayout user "Dashboard" $
    H.div $ do
      H.h1 ! A.class_ (stringValue "page-title") $ H.toHtml "Dashboard"

      -- User info panel
      H.div ! A.class_ (stringValue "panel") $ do
        H.div ! A.class_ (stringValue "panel-title") $ H.toHtml "Player Info"
        H.div ! A.class_ (stringValue "info-grid") $ do
          H.div ! A.class_ (stringValue "info-item") $ do
            H.strong $ H.toHtml "Name"
            H.span $ H.toHtml (User.AuthenticatedUser.name user)
          H.div ! A.class_ (stringValue "info-item") $ do
            H.strong $ H.toHtml "Email"
            H.span $ H.toHtml (email user)
          H.div ! A.class_ (stringValue "info-item") $ do
            H.strong $ H.toHtml "User ID"
            H.span $ H.toHtml (show (auId user))
          H.div ! A.class_ (stringValue "info-item") $ do
            H.strong $ H.toHtml "Collection"
            H.span $ H.toHtml (show (length (personalCollection user)) ++ " cards")
            H.toHtml (" " :: String)
            H.a
              ! A.href (stringValue "/personal-collection")
              ! A.style (stringValue "color: #8b1a1a; font-family: 'Arial Black', sans-serif; font-size: 0.75rem; text-transform: uppercase; letter-spacing: 1px;")
              $ H.toHtml "[View]"

      -- Game frame — loads via HTMX on page load
      H.div
        ! A.id (stringValue "game-shell")
        ! Htmx.hxGet (stringValue "/game-frame")
        ! Htmx.hxTrigger (stringValue "load")
        ! Htmx.hxSwap (stringValue "innerHTML")
        ! A.class_ (stringValue "game-shell panel")
        $ H.p
          ! A.style (stringValue "text-align: center; color: #8b1a1a; font-family: 'Arial Black', sans-serif; text-transform: uppercase; letter-spacing: 2px; font-size: 0.75rem; padding: 40px 0;")
        $ H.toHtml "Loading..."
