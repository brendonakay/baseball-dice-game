{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module User.Account where

import Data.Aeson
import GHC.Generics
import WaxBall.Card (Card)

data User = User
  { userId :: Int,
    userName :: String,
    userEmail :: String,
    cards :: [Card]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
