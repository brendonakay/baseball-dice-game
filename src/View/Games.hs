module View.Games where

import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import User.AuthenticatedUser (AuthenticatedUser)
import View.Layout (mainLayout)

gamesPageToHtml :: AuthenticatedUser -> Html
gamesPageToHtml user =
  mainLayout user "Games" $
    H.div $ do
      H.h1 ! A.class_ (stringValue "page-title") $ H.toHtml "Games"
      H.div ! A.class_ (stringValue "panel") $ do
        H.p ! A.class_ (stringValue "empty-state") $ do
          H.toHtml "Coming soon — game history and statistics will appear here."
