{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module User.AuthenticatedUser where

import Data.Aeson
import Data.IORef
import GHC.Generics
import WaxBall.Card (Card)

data AuthenticatedUser = User
  { auId :: Int,
    name :: String,
    email :: String,
    personalCollection :: [Card]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Reference type for thread-safe user state management
type UserRef = IORef (Maybe AuthenticatedUser)
