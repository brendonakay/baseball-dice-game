module View.Auth where

import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx as Htmx

loginPageHtml :: Html
loginPageHtml = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml "Baseball Dice Game - Login"
    H.meta ! A.charset (stringValue "utf-8")
    H.meta ! A.name (stringValue "viewport") ! A.content (stringValue "width=device-width, initial-scale=1")
    H.script ! A.src (stringValue "https://unpkg.com/htmx.org@1.9.10") $ H.toHtml ""
  H.body ! A.style (stringValue "background: #f5f5f5; font-family: Arial, sans-serif; margin: 0; padding: 0; min-height: 100vh;") $ do
    H.div ! A.style (stringValue "max-width: 400px; margin: 50px auto; padding: 20px; background: white; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);") $ do
      H.h1 ! A.style (stringValue "text-align: center; color: #2c3e50; margin-bottom: 30px;") $ H.toHtml "Baseball Dice Game"

      H.h2 ! A.style (stringValue "color: #3498db; border-bottom: 2px solid #3498db; padding-bottom: 10px;") $ H.toHtml "Login"
      H.form ! Htmx.hxPost (stringValue "/login") ! Htmx.hxTarget (stringValue "body") $ do
        H.div ! A.style (stringValue "margin-bottom: 15px;") $ do
          H.label ! A.for (stringValue "username") ! A.style (stringValue "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Username:"
          H.input ! A.type_ (stringValue "text") ! A.name (stringValue "username") ! A.id (stringValue "username") ! A.required (stringValue "") ! A.style (stringValue "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
        H.div ! A.style (stringValue "margin-bottom: 15px;") $ do
          H.label ! A.for (stringValue "password") ! A.style (stringValue "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Password:"
          H.input ! A.type_ (stringValue "password") ! A.name (stringValue "password") ! A.id (stringValue "password") ! A.required (stringValue "") ! A.style (stringValue "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
        H.button ! A.type_ (stringValue "submit") ! A.style (stringValue "width: 100%; padding: 10px; background: #3498db; color: white; border: none; border-radius: 4px; cursor: pointer; font-size: 16px;") $ H.toHtml "Login"

      H.h2 ! A.style (stringValue "color: #27ae60; border-bottom: 2px solid #27ae60; padding-bottom: 10px; margin-top: 30px;") $ H.toHtml "Register"
      H.form ! Htmx.hxPost (stringValue "/register") ! Htmx.hxTarget (stringValue "body") $ do
        H.div ! A.style (stringValue "margin-bottom: 15px;") $ do
          H.label ! A.for (stringValue "reg_username") ! A.style (stringValue "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Username:"
          H.input ! A.type_ (stringValue "text") ! A.name (stringValue "username") ! A.id (stringValue "reg_username") ! A.required (stringValue "") ! A.style (stringValue "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
        H.div ! A.style (stringValue "margin-bottom: 15px;") $ do
          H.label ! A.for (stringValue "reg_email") ! A.style (stringValue "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Email:"
          H.input ! A.type_ (stringValue "email") ! A.name (stringValue "email") ! A.id (stringValue "reg_email") ! A.required (stringValue "") ! A.style (stringValue "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
        H.div ! A.style (stringValue "margin-bottom: 15px;") $ do
          H.label ! A.for (stringValue "reg_password") ! A.style (stringValue "display: block; margin-bottom: 5px; font-weight: bold;") $ H.toHtml "Password:"
          H.input ! A.type_ (stringValue "password") ! A.name (stringValue "password") ! A.id (stringValue "reg_password") ! A.required (stringValue "") ! A.style (stringValue "width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box;")
        H.button ! A.type_ (stringValue "submit") ! A.style (stringValue "width: 100%; padding: 10px; background: #27ae60; color: white; border: none; border-radius: 4px; cursor: pointer; font-size: 16px;") $ H.toHtml "Register"

loginSuccessRedirectHtml :: Html
loginSuccessRedirectHtml = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml "Login Successful"
    H.script ! A.src (stringValue "https://unpkg.com/htmx.org@1.9.10") $ H.toHtml ""
  H.body $ do
    H.p $ H.toHtml "Login successful! Redirecting..."
    H.script $
      H.toHtml $
        unlines
          [ "// Redirect to user page",
            "window.location.href = '/user';"
          ]

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
