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
  | Strike

-- TODO: Maybe there's a smarter way to do this.
-- Or something better than Maybe.
getStrikeAction :: Int -> Int -> Maybe StrikeAction
getStrikeAction a b =
  case (a, b) of
    (1, 1) -> Just HitDouble
    (1, 2) -> Just GroundOut
    (1, 3) -> Just HitByPitch
    (1, 4) -> Just HitSingle
    (1, 5) -> Just GroundOut
    (1, 6) -> Just Strike
    (2, 2) -> Just HitDouble
    (2, 3) -> Just PopOut
    (2, 4) -> Just HitSingle
    (2, 5) -> Just Strike
    (2, 6) -> Just GroundOut
    (3, 3) -> Just HitTriple
    (3, 4) -> Just Strike
    (3, 5) -> Just GroundOut
    (3, 6) -> Just FlyOut
    (4, 4) -> Just FieldingError
    (4, 5) -> Just FlyOut
    (4, 6) -> Just FlyOut
    (5, 5) -> Just HitSingle
    (5, 6) -> Just PopOut
    (6, 6) -> Just HomeRun
    (_, _) -> Nothing
