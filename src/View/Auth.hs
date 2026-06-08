module View.Auth where

import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx as Htmx

loginPageHtml :: Html
loginPageHtml = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml "WaxBall - Login"
    H.meta ! A.charset (stringValue "utf-8")
    H.meta ! A.name (stringValue "viewport") ! A.content (stringValue "width=device-width, initial-scale=1")
    H.link ! A.rel (stringValue "stylesheet") ! A.href (stringValue "/static/css/app.css")
    H.script ! A.src (stringValue "/static/vendor/htmx.min.js") $ mempty
    H.script ! A.src (stringValue "/static/js/app.js") ! A.defer (stringValue "") $ mempty
  H.body ! A.class_ (stringValue "auth-page") $ do
    H.div ! A.class_ (stringValue "auth-card") $ do
      H.h1 ! A.class_ (stringValue "auth-title") $ H.toHtml "WaxBall"

      H.h2 ! A.class_ (stringValue "auth-heading") $ H.toHtml "Login"
      H.form ! Htmx.hxPost (stringValue "/login") ! Htmx.hxTarget (stringValue "body") $ do
        H.div ! A.class_ (stringValue "form-group") $ do
          H.label ! A.for (stringValue "username") ! A.class_ (stringValue "form-label") $ H.toHtml "Username"
          H.input ! A.type_ (stringValue "text") ! A.name (stringValue "username") ! A.id (stringValue "username") ! A.required (stringValue "") ! A.class_ (stringValue "form-input")
        H.div ! A.class_ (stringValue "form-group") $ do
          H.label ! A.for (stringValue "password") ! A.class_ (stringValue "form-label") $ H.toHtml "Password"
          H.input ! A.type_ (stringValue "password") ! A.name (stringValue "password") ! A.id (stringValue "password") ! A.required (stringValue "") ! A.class_ (stringValue "form-input")
        H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "btn-primary") $ H.toHtml "Login"

      H.h2 ! A.class_ (stringValue "auth-heading auth-heading-register") $ H.toHtml "Register"
      H.form ! Htmx.hxPost (stringValue "/register") ! Htmx.hxTarget (stringValue "body") $ do
        H.div ! A.class_ (stringValue "form-group") $ do
          H.label ! A.for (stringValue "reg_username") ! A.class_ (stringValue "form-label") $ H.toHtml "Username"
          H.input ! A.type_ (stringValue "text") ! A.name (stringValue "username") ! A.id (stringValue "reg_username") ! A.required (stringValue "") ! A.class_ (stringValue "form-input")
        H.div ! A.class_ (stringValue "form-group") $ do
          H.label ! A.for (stringValue "reg_email") ! A.class_ (stringValue "form-label") $ H.toHtml "Email"
          H.input ! A.type_ (stringValue "email") ! A.name (stringValue "email") ! A.id (stringValue "reg_email") ! A.required (stringValue "") ! A.class_ (stringValue "form-input")
        H.div ! A.class_ (stringValue "form-group") $ do
          H.label ! A.for (stringValue "reg_password") ! A.class_ (stringValue "form-label") $ H.toHtml "Password"
          H.input ! A.type_ (stringValue "password") ! A.name (stringValue "password") ! A.id (stringValue "reg_password") ! A.required (stringValue "") ! A.class_ (stringValue "form-input")
        H.button ! A.type_ (stringValue "submit") ! A.class_ (stringValue "btn-success") $ H.toHtml "Register"

-- Returned on successful login. Navigation is handled by the HX-Redirect
-- response header (see loginHandler), so no inline redirect script is needed;
-- the link below is just a no-JS fallback.
loginSuccessRedirectHtml :: Html
loginSuccessRedirectHtml = H.docTypeHtml $ do
  H.head $ H.title $ H.toHtml "Login Successful"
  H.body $ do
    H.p $ H.toHtml "Login successful! Redirecting..."
    H.a ! A.href (stringValue "/user") $ H.toHtml "Continue"

loginFailedHtml :: Html
loginFailedHtml = H.docTypeHtml $ do
  H.head $ H.title $ H.toHtml "Login Failed"
  H.body $ do
    H.h1 $ H.toHtml "Login Failed"
    H.p $ H.toHtml "Invalid username or password."
    H.a ! A.href (stringValue "/") $ H.toHtml "Try again"

loginErrorHtml :: Html
loginErrorHtml = H.docTypeHtml $ do
  H.head $ H.title $ H.toHtml "Login Error"
  H.body $ do
    H.h1 $ H.toHtml "Login Error"
    H.p $ H.toHtml "Missing username or password."
    H.a ! A.href (stringValue "/") $ H.toHtml "Try again"

registrationFailedHtml :: String -> Html
registrationFailedHtml errorMsg = H.docTypeHtml $ do
  H.head $ H.title $ H.toHtml "Registration Failed"
  H.body $ do
    H.h1 $ H.toHtml "Registration Failed"
    H.p $ H.toHtml errorMsg
    H.a ! A.href (stringValue "/") $ H.toHtml "Try again"

registrationSuccessHtml :: Html
registrationSuccessHtml = H.docTypeHtml $ do
  H.head $ H.title $ H.toHtml "Registration Successful"
  H.body $ do
    H.h1 $ H.toHtml "Registration Successful"
    H.p $ H.toHtml "You can now login with your credentials."
    H.a ! A.href (stringValue "/") $ H.toHtml "Login"

registrationErrorHtml :: Html
registrationErrorHtml = H.docTypeHtml $ do
  H.head $ H.title $ H.toHtml "Registration Error"
  H.body $ do
    H.h1 $ H.toHtml "Registration Error"
    H.p $ H.toHtml "Missing required fields."
    H.a ! A.href (stringValue "/") $ H.toHtml "Try again"

logoutHtml :: Html
logoutHtml = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.httpEquiv (stringValue "refresh") ! A.content (stringValue "0;url=/")
    H.title $ H.toHtml "Logged Out"
  H.body $ do
    H.p $ H.toHtml "Logged out successfully. Redirecting to login..."
