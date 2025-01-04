{-# LANGUAGE CPP #-}

-- This module is abandoned. I am no longer persuing the TUI version of the game. However, I wanted to keep this code
-- for the sake of reference, or if I feel inspired to resurrect this effort.

module UI.TUI where

import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Brick.AttrMap
  ( attrMap,
  )
import qualified Brick.Main as M
import Brick.Types
  ( ViewportType (Both),
    Widget,
  )
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( hLimit,
    str,
    txt,
    vBox,
    vLimit,
    viewport,
  )
import Brick.Widgets.Table as Tbl
import Data.Text (pack)
import Game.Logic
import qualified Graphics.Vty as V

ui :: [[String]] -> Widget ()
ui pl =
  C.center $
    Tbl.renderTable (renderPitchLog pl)

renderPitchLog :: [[String]] -> Table ()
renderPitchLog plr =
  alignCenter 1 $
    alignRight 2 $
      alignMiddle 2 $
        table
          [ [Tbl.renderTable (pitchLogTable plr)]
          ]

pitchLogTable :: [[String]] -> Table ()
pitchLogTable plr =
  alignCenter 1 $
    table $
      map (txt . pack) logFields -- Header
        : map (map (txt . pack)) plr -- Rows

-- Entrypoint to TUI app.
-- PitchLog is passed in as a parameter. Not ideal.
runGameTableTUI :: [[String]] -> IO ()
runGameTableTUI pl = void $ M.defaultMain (app pl) ()

drawUi :: [[String]] -> () -> [Widget ()]
drawUi plr = const [ui']
  where
    ui' =
      C.center $
        B.border $
          hLimit 100 $
            vLimit 100 $
              vBox [B.hBorder, singleton]
    singleton =
      viewport () Both $
        vBox $
          Tbl.renderTable (pitchLogTable plr)
            : (str <$> ["Line " <> show i | i <- [3 .. 50 :: Int]])

gameViewportScroll :: M.ViewportScroll ()
gameViewportScroll = M.viewportScroll ()

appEvent :: T.BrickEvent () e -> T.EventM () () ()
appEvent (T.VtyEvent (V.EvKey V.KDown [])) = M.vScrollBy gameViewportScroll 1
appEvent (T.VtyEvent (V.EvKey V.KUp [])) = M.vScrollBy gameViewportScroll (-1)
appEvent (T.VtyEvent (V.EvKey V.KRight [])) = M.hScrollBy gameViewportScroll 1
appEvent (T.VtyEvent (V.EvKey V.KLeft [])) = M.hScrollBy gameViewportScroll (-1)
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent _ = pure ()

app :: [[String]] -> M.App () e ()
app plr =
  M.App
    { M.appDraw = drawUi plr,
      M.appStartEvent = pure (),
      M.appHandleEvent = appEvent,
      M.appAttrMap = const $ attrMap V.defAttr [],
      M.appChooseCursor = M.neverShowCursor
    }
