module API.Handlers where

import Text.Blaze.Html5 as H

getGameDataRow :: Html
getGameDataRow = H.tr $
  do
    H.td $ H.toHtml "0" -- Home Score
    H.td $ H.toHtml "0" -- Away Score
    H.td $ H.toHtml "0" -- Inning
    H.td $ H.toHtml "0" -- Half Inning
    H.td $ H.toHtml "0" -- Current Batter
    H.td $ H.toHtml "0" -- Balls
    H.td $ H.toHtml "0" -- Strikes
    H.td $ H.toHtml "0" -- Outs
