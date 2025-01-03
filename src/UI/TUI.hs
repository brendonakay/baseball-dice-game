{-# LANGUAGE CPP #-}

module UI.TUI where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Table
import Data.Text (pack)
import Game.Logic

ui :: [[String]] -> Widget ()
ui pl =
  center $
    renderTable (renderPitchLog pl)

renderPitchLog :: [[String]] -> Table ()
renderPitchLog plr =
  alignCenter 1 $
    alignRight 2 $
      alignMiddle 2 $
        table
          [ [renderTable (pitchLogTable plr)]
          ]

pitchLogTable :: [[String]] -> Table ()
pitchLogTable plr =
  alignCenter 1 $
    table $
      map (txt . pack) logFields -- Header
        : map (map (txt . pack)) plr -- Rows

runGameTableTUI :: [[String]] -> IO ()
runGameTableTUI pl = simpleMain (ui pl)
