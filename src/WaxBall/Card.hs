{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module WaxBall.Card where

import Data.Aeson
import GHC.Generics
import WaxBall.Game (Player)

data Card = Card
  { id :: Int,
    number :: String,
    player :: Player,
    team :: String,
    cardType :: Type -- type is a reserved word in Haskell
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Type = Base | Insert | Parallel | Autograph
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
