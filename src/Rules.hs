module Rules where

data StrikeAction
  = FieldingError
  | FlyOut
  | GroundOut
  | HitByPitch
  | HitDouble
  | HitSingle
  | HitTriple
  | HomeRun
  | PopOut
  | CalledStrike
  | NoAction
  deriving (Show)

getStrikeAction :: Int -> Int -> StrikeAction
getStrikeAction a b =
  case (a, b) of
    (1, 1) -> HitDouble
    (1, 2) -> GroundOut
    (1, 3) -> HitByPitch
    (1, 4) -> HitSingle
    (1, 5) -> GroundOut
    (1, 6) -> CalledStrike
    (2, 2) -> HitDouble
    (2, 3) -> PopOut
    (2, 4) -> HitSingle
    (2, 5) -> CalledStrike
    (2, 6) -> GroundOut
    (3, 3) -> HitTriple
    (3, 4) -> CalledStrike
    (3, 5) -> GroundOut
    (3, 6) -> FlyOut
    (4, 4) -> FieldingError
    (4, 5) -> FlyOut
    (4, 6) -> FlyOut
    (5, 5) -> HitSingle
    (5, 6) -> PopOut
    (6, 6) -> HomeRun
    (_, _) -> NoAction
