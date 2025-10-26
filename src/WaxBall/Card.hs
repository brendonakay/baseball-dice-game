{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module WaxBall.Card where

import Data.Aeson
import GHC.Generics
import WaxBall.Game (Player)

-- TODO:
-- - Rarity is determined by pop counts
-- - (?) Should a card know its set?
data Card = Card
  { id :: Int,
    number :: String,
    player :: Player,
    team :: String,
    cardType :: Type -- type is a reserved word in Haskell
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- TODO: Maybe rename Special to something else?
data Special = Autograph | Serial
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Class = Base | Insert | Parallel
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Type = Type Class (Maybe Special)
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Set = Set
  { name :: String,
    list :: [Card]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
