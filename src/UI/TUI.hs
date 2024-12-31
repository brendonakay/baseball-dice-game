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
          [ [renderTable pitchLogHeader],
            [renderTable (pitchLogRows plr)]
          ]

-- BUG: This seems to be returning a single list with one long element.
--  It makes it so the UI has just one row with the whole game log appended.
pitchLogRows :: [[String]] -> Table ()
pitchLogRows plr =
  alignCenter 1 $
    table
      [concatMap (map (txt . pack)) plr]

pitchLogHeader :: Table ()
pitchLogHeader =
  alignCenter 1 $
    table
      [map (txt . pack) logFields]

runGameTableTUI :: [[String]] -> IO ()
runGameTableTUI pl = simpleMain (ui pl)
