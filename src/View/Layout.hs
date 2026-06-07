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
    H.script ! A.src (stringValue "https://unpkg.com/htmx.org@1.9.10") $ H.toHtml ""
    H.style $ H.toHtml globalCSS
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
          ! A.style (stringValue "display:inline;")
          $ H.button
            ! A.type_ (stringValue "submit")
            ! A.class_ (stringValue "nav-logout-btn")
          $ H.toHtml "Logout"

globalCSS :: String
globalCSS =
  unlines
    [ "* { box-sizing: border-box; margin: 0; padding: 0; }",
      "body { background: #f5e6c8; font-family: Georgia, serif; color: #1a2744; min-height: 100vh; }",
      "a { color: #1a2744; text-decoration: none; }",
      -- Nav
      ".nav-bar { background: #1a2744; border-bottom: 4px solid #c9a227; position: sticky; top: 0; z-index: 100; }",
      ".nav-inner { max-width: 1100px; margin: 0 auto; padding: 0 24px; display: flex; align-items: center; justify-content: space-between; height: 56px; }",
      ".nav-logo { font-family: 'Arial Black', Impact, sans-serif; color: #c9a227; letter-spacing: 3px; text-transform: uppercase; font-size: 1.4rem; }",
      ".nav-links { display: flex; gap: 24px; }",
      ".nav-link { font-family: 'Arial Black', sans-serif; color: #faf8f2; text-transform: uppercase; letter-spacing: 2px; font-size: 0.75rem; text-decoration: none; padding-bottom: 2px; border-bottom: 3px solid transparent; transition: color 0.15s, border-color 0.15s; }",
      ".nav-link:hover { border-bottom: 3px solid #c9a227; color: #c9a227; }",
      ".nav-user { display: flex; align-items: center; gap: 12px; }",
      ".nav-username { font-family: 'Arial Black', sans-serif; color: #c9a227; font-size: 0.75rem; letter-spacing: 1px; text-transform: uppercase; }",
      ".nav-logout-btn { background: transparent; border: 2px solid #c9a227; color: #c9a227; font-family: 'Arial Black', sans-serif; font-size: 0.65rem; letter-spacing: 1px; text-transform: uppercase; padding: 4px 10px; cursor: pointer; transition: background 0.15s, color 0.15s; }",
      ".nav-logout-btn:hover { background: #c9a227; color: #1a2744; }",
      -- Layout
      ".page-content { max-width: 1100px; margin: 0 auto; padding: 24px; }",
      -- Page headings
      ".page-title { font-family: 'Arial Black', Impact, sans-serif; color: #1a2744; text-transform: uppercase; letter-spacing: 3px; border-bottom: 4px solid #c9a227; padding-bottom: 12px; margin-bottom: 24px; font-size: 1.6rem; }",
      -- Panel cards
      ".panel { background: #faf8f2; border: 2px solid #1a2744; padding: 20px; margin-bottom: 20px; box-shadow: 3px 3px 0 #c9a227; }",
      ".panel-title { font-family: 'Arial Black', sans-serif; color: #1a2744; text-transform: uppercase; letter-spacing: 2px; font-size: 1rem; border-bottom: 2px solid #c9a227; padding-bottom: 8px; margin-bottom: 16px; }",
      -- Info grid
      ".info-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; }",
      ".info-item { padding: 10px 12px; background: #f0e8d0; border-left: 3px solid #c9a227; font-size: 0.9rem; }",
      ".info-item strong { color: #8b1a1a; font-family: 'Arial Black', sans-serif; font-size: 0.75rem; text-transform: uppercase; letter-spacing: 1px; display: block; margin-bottom: 3px; }",
      -- Buttons
      ".btn-primary { background: #1a2744; color: #c9a227; border: 2px solid #c9a227; font-family: 'Arial Black', sans-serif; text-transform: uppercase; letter-spacing: 2px; font-size: 0.8rem; padding: 10px 20px; cursor: pointer; transition: background 0.15s, color 0.15s; }",
      ".btn-primary:hover { background: #c9a227; color: #1a2744; }",
      ".btn-danger { background: #8b1a1a; color: #faf8f2; border: 2px solid #8b1a1a; font-family: 'Arial Black', sans-serif; text-transform: uppercase; letter-spacing: 2px; font-size: 0.8rem; padding: 10px 20px; cursor: pointer; transition: background 0.15s; }",
      ".btn-danger:hover { background: #6b1414; }",
      ".btn-success { background: #1a4427; color: #faf8f2; border: 2px solid #1a4427; font-family: 'Arial Black', sans-serif; text-transform: uppercase; letter-spacing: 2px; font-size: 0.8rem; padding: 10px 20px; cursor: pointer; }",
      ".btn-success:hover { background: #143520; }",
      -- Tables
      ".data-table { width: 100%; border-collapse: collapse; }",
      ".data-table th { background: #1a2744; color: #c9a227; font-family: 'Arial Black', sans-serif; text-transform: uppercase; letter-spacing: 1px; font-size: 0.7rem; padding: 10px; text-align: left; }",
      ".data-table td { padding: 10px; border-bottom: 1px solid #c9a227; font-size: 0.9rem; }",
      ".data-table tr:nth-child(even) td { background: #f0e8d0; }",
      -- Season / Game results
      ".game-result-row { padding: 12px; margin: 8px 0; background: #faf8f2; border-left: 4px solid #1a2744; }",
      ".game-result-row .winner { color: #1a4427; font-family: 'Arial Black', sans-serif; }",
      -- Progress bar
      ".progress-bar { width: 100%; height: 12px; background: #d4c5a0; border: 1px solid #c9a227; }",
      ".progress-fill { height: 100%; background: #1a2744; transition: width 0.3s ease; }",
      -- Config form
      ".config-form-row { margin: 8px 0; display: flex; align-items: center; gap: 10px; }",
      ".config-form-row label { min-width: 140px; font-family: 'Arial Black', sans-serif; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 1px; color: #8b1a1a; }",
      ".config-form-row input { padding: 5px 8px; border: 1px solid #1a2744; background: #faf8f2; font-family: Georgia, serif; flex: 1; }",
      ".update-btn { background: #1a4427; color: #faf8f2; border: 1px solid #1a4427; font-family: 'Arial Black', sans-serif; font-size: 0.65rem; text-transform: uppercase; letter-spacing: 1px; padding: 4px 10px; cursor: pointer; margin-top: 8px; }",
      ".update-btn:hover { background: #143520; }",
      -- Player form card
      ".player-form { background: #faf8f2; border: 1px solid #c9a227; padding: 14px; margin: 12px 0; border-left: 4px solid #1a2744; }",
      ".player-form h3 { font-family: 'Arial Black', sans-serif; font-size: 0.85rem; text-transform: uppercase; letter-spacing: 1px; color: #1a2744; margin-bottom: 10px; }",
      -- Game frame
      ".game-shell { min-height: 300px; }",
      ".game-frame { font-family: Georgia, serif; }",
      ".scoreboard { display: flex; justify-content: space-between; background: #1a2744; color: #faf8f2; padding: 15px 20px; margin-bottom: 16px; border-bottom: 4px solid #c9a227; }",
      ".score-section h3 { margin: 0 0 4px 0; font-family: 'Arial Black', sans-serif; font-size: 0.75rem; text-transform: uppercase; letter-spacing: 2px; color: #c9a227; }",
      ".score-section .score { font-size: 2.2em; font-weight: bold; font-family: 'Arial Black', sans-serif; }",
      ".game-info { text-align: center; margin-bottom: 16px; font-family: 'Arial Black', sans-serif; text-transform: uppercase; letter-spacing: 2px; color: #1a2744; }",
      ".diamond-container { position: relative; width: 280px; height: 280px; margin: 0 auto; }",
      ".diamond { width: 180px; height: 180px; background: #8B4513; transform: rotate(45deg); position: absolute; top: 50px; left: 50px; border-radius: 10px; }",
      ".base { position: absolute; width: 20px; height: 20px; background: #faf8f2; border: 2px solid #1a2744; }",
      ".base.occupied { background: #c9a227; }",
      ".first-base { top: 130px; right: 40px; }",
      ".second-base { top: 40px; right: 130px; }",
      ".third-base { top: 130px; left: 40px; }",
      ".home-plate { bottom: 40px; left: 130px; border-radius: 50%; }",
      ".batter-info { text-align: center; margin: 16px 0; padding: 12px; background: #f0e8d0; border: 1px solid #c9a227; font-size: 0.9rem; }",
      ".count { display: flex; justify-content: center; gap: 24px; margin-top: 16px; }",
      ".count-item { text-align: center; }",
      ".count-item .number { font-size: 2em; font-weight: bold; color: #1a2744; font-family: 'Arial Black', sans-serif; }",
      ".count-item .label { font-size: 0.75rem; color: #8b1a1a; font-family: 'Arial Black', sans-serif; text-transform: uppercase; letter-spacing: 1px; }",
      ".game-log { background: #faf8f2; border: 1px solid #1a2744; padding: 16px; }",
      ".log-entries { max-height: 500px; overflow-y: auto; }",
      -- 1960s Baseball Card styles
      ".pc-cards-grid { display: flex; flex-wrap: wrap; gap: 24px; justify-content: center; padding: 20px 0; }",
      -- Flip container (outer)
      ".pc-card-flip { width: 200px; aspect-ratio: 5/7; perspective: 1000px; box-shadow: 3px 3px 0 #8b1a1a, 6px 6px 0 #1a2744; transition: transform 0.15s ease; cursor: pointer; }",
      ".pc-card-flip:hover { transform: translateY(-5px) rotate(-0.5deg); }",
      -- Flip inner (rotates on hover)
      ".pc-card-flipper { position: relative; width: 100%; height: 100%; transform-style: preserve-3d; transition: transform 0.55s cubic-bezier(0.4, 0, 0.2, 1); }",
      ".pc-card-flip:hover .pc-card-flipper { transform: rotateY(180deg); }",
      -- Shared face styles
      ".pc-card-face { position: absolute; inset: 0; backface-visibility: hidden; -webkit-backface-visibility: hidden; border: 3px solid #1a2744; background: #faf8f2; overflow: hidden; display: flex; flex-direction: column; }",
      -- Back face starts rotated 180deg (hidden until hover)
      ".pc-card-back { transform: rotateY(180deg); }",
      -- Shared child element styles (used on both faces)
      ".pc-card-header { background: #1a2744; padding: 8px 10px; display: flex; justify-content: space-between; align-items: flex-start; flex-shrink: 0; }",
      ".pc-set-name { color: #c9a227; font-family: 'Arial Black', sans-serif; font-size: 0.5rem; letter-spacing: 2px; text-transform: uppercase; }",
      ".pc-jersey { color: #faf8f2; font-family: 'Arial Black', sans-serif; font-size: 2rem; line-height: 1; }",
      ".pc-photo-area { background: linear-gradient(135deg, #8b1a1a, #6b1414); height: 55px; display: flex; align-items: center; justify-content: center; color: #f5e6c8; font-family: 'Arial Black', sans-serif; font-size: 0.6rem; letter-spacing: 2px; flex-shrink: 0; }",
      ".pc-card-body { padding: 8px 10px; background: #faf8f2; flex: 1; }",
      ".pc-player-name { font-family: 'Arial Black', sans-serif; font-size: 0.8rem; text-transform: uppercase; letter-spacing: 1px; color: #1a2744; border-bottom: 2px solid #c9a227; padding-bottom: 4px; margin-bottom: 6px; }",
      ".pc-team { font-size: 0.6rem; color: #8b1a1a; font-family: 'Arial Black', sans-serif; letter-spacing: 2px; text-transform: uppercase; margin-bottom: 6px; }",
      ".pc-stats-row { display: grid; grid-template-columns: repeat(3, 1fr); gap: 3px; margin-top: 6px; }",
      ".pc-stat { text-align: center; background: #f0e8d0; border: 1px solid #c9a227; padding: 3px 2px; }",
      ".pc-stat-label { display: block; font-size: 0.45rem; color: #8b1a1a; font-family: 'Arial Black', sans-serif; text-transform: uppercase; }",
      ".pc-stat-value { display: block; font-size: 0.8rem; font-weight: bold; color: #1a2744; font-family: 'Courier New', monospace; }",
      ".pc-type-badge { position: absolute; bottom: 8px; right: 8px; background: #c9a227; color: #1a2744; font-family: 'Arial Black', sans-serif; font-size: 0.4rem; padding: 2px 4px; letter-spacing: 1px; text-transform: uppercase; }",
      -- Front face specific styles
      ".pc-front-photo { flex: 1; background: linear-gradient(180deg, #d4b896 0%, #c4a070 55%, #b08050 100%); display: flex; flex-direction: column; align-items: center; justify-content: center; position: relative; overflow: hidden; }",
      ".pc-front-jersey-bg { font-family: 'Arial Black', Impact, sans-serif; font-size: 5.5rem; font-weight: 900; color: rgba(26,39,68,0.10); line-height: 1; position: absolute; user-select: none; pointer-events: none; }",
      ".pc-front-photo-icon { font-size: 2.2rem; position: relative; z-index: 1; opacity: 0.5; }",
      ".pc-front-photo-label { font-family: 'Arial Black', sans-serif; font-size: 0.42rem; text-transform: uppercase; letter-spacing: 3px; color: rgba(26,39,68,0.35); margin-top: 5px; position: relative; z-index: 1; }",
      ".pc-front-footer { background: #1a2744; padding: 8px 10px; flex-shrink: 0; border-top: 3px solid #c9a227; }",
      ".pc-front-name { font-family: 'Arial Black', sans-serif; font-size: 0.75rem; text-transform: uppercase; letter-spacing: 1px; color: #faf8f2; line-height: 1.2; }",
      ".pc-front-team-label { font-size: 0.5rem; font-family: 'Arial Black', sans-serif; color: #c9a227; text-transform: uppercase; letter-spacing: 2px; margin-top: 2px; }",
      -- Empty state
      ".empty-state { text-align: center; padding: 60px 20px; }",
      ".empty-state p { font-family: 'Arial Black', sans-serif; color: #8b1a1a; letter-spacing: 2px; text-transform: uppercase; font-size: 1rem; }"
    ]
