{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module User.AuthenticatedUser where

import Data.Aeson
import GHC.Generics
import Servant.Auth.Server (FromJWT, ToJWT)
import WaxBall.Card (Card)

data AuthenticatedUser = User
  { auId :: Int,
    name :: String,
    email :: String,
    personalCollection :: [Card]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToJWT, FromJWT)
