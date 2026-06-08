module View.Layout where

import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import User.AuthenticatedUser (AuthenticatedUser (..))

mainLayout :: AuthenticatedUser -> String -> Html -> Html
mainLayout user title content = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset (stringValue "UTF-8")
    H.meta
      ! A.name (stringValue "viewport")
      ! A.content (stringValue "width=device-width, initial-scale=1.0")
    H.title $ H.toHtml title
    H.link ! A.rel (stringValue "stylesheet") ! A.href (stringValue "/static/css/app.css")
    H.script ! A.src (stringValue "/static/vendor/htmx.min.js") $ mempty
    H.script ! A.src (stringValue "/static/js/app.js") ! A.defer (stringValue "") $ mempty
  H.body $ do
    navBar user
    H.div ! A.class_ (stringValue "page-content") $ content

navBar :: AuthenticatedUser -> Html
navBar user =
  H.nav ! A.class_ (stringValue "nav-bar") $
    H.div ! A.class_ (stringValue "nav-inner") $ do
      H.a ! A.href (stringValue "/user") ! A.class_ (stringValue "nav-logo") $
        H.toHtml "WaxBall"
      H.div ! A.class_ (stringValue "nav-links") $ do
        H.a
          ! A.href (stringValue "/personal-collection")
          ! A.class_ (stringValue "nav-link")
          $ H.toHtml "Collection"
        H.a
          ! A.href (stringValue "/season")
          ! A.class_ (stringValue "nav-link")
          $ H.toHtml "Season"
        H.a
          ! A.href (stringValue "/games")
          ! A.class_ (stringValue "nav-link")
          $ H.toHtml "Games"
      H.div ! A.class_ (stringValue "nav-user") $ do
        H.span ! A.class_ (stringValue "nav-username") $
          H.toHtml (User.AuthenticatedUser.name user)
        H.form
          ! A.action (stringValue "/logout")
          ! A.method (stringValue "post")
          ! A.class_ (stringValue "inline-form")
          $ H.button
            ! A.type_ (stringValue "submit")
            ! A.class_ (stringValue "nav-logout-btn")
          $ H.toHtml "Logout"
