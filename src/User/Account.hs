{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module User.Account where

import Data.Aeson
import Data.IORef
import GHC.Generics
import WaxBall.Card (Card)

data User = User
  { userId :: Int,
    userName :: String,
    userEmail :: String,
    cards :: [Card]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Reference type for thread-safe user state management
type UserRef = IORef User
