{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module WaxBall.Card where

import Data.Aeson
import GHC.Generics
import WaxBall.Game (Player)

data Card = Card
  { id :: Int,
    number :: String,
    player :: Player
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
