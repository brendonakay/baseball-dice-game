module View.Config where

import qualified Data.List as List
import Data.Maybe (fromMaybe)
import WaxBall.Game (Player (..))

-- Update a single player in a team list at the given index, clamping batting
-- average and slugging percentage to their realistic ranges. Used by the
-- /update-player handler.
updatePlayerAtIndex :: [Player] -> Int -> Maybe String -> Maybe Int -> Maybe Double -> Maybe Double -> [Player]
updatePlayerAtIndex players idx mName mNumber mBattingAvg mSlugging =
  let updatePlayer player =
        player
          { name = fromMaybe (WaxBall.Game.name player) mName,
            number = fromMaybe (WaxBall.Game.number player) mNumber,
            battingAverage = maybe (battingAverage player) (Prelude.max 0.150 . Prelude.min 0.400) mBattingAvg,
            sluggingPercentage = maybe (sluggingPercentage player) (Prelude.max 0.300 . Prelude.min 0.700) mSlugging
          }
   in case List.splitAt idx players of
        (before, player : after) -> before ++ [updatePlayer player] ++ after
        _ -> players
