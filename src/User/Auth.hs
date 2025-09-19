{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module User.Auth where

import Control.Monad (when)
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import GHC.Generics
import User.AuthenticatedUser (AuthenticatedUser (..))
import WaxBall.Card (Card)

-- Database user representation
data DbUser = DbUser
  { dbUserId :: Int,
    dbUsername :: Text,
    dbEmail :: Text,
    dbPassword :: Text
  }
  deriving (Show, Eq, Generic)

instance FromRow DbUser where
  fromRow = DbUser <$> field <*> field <*> field <*> field

-- Login credentials
data LoginCredentials = LoginCredentials
  { loginUsername :: Text,
    loginPassword :: Text
  }
  deriving (Show, Eq, Generic)

-- Registration data
data RegisterData = RegisterData
  { regUsername :: Text,
    regEmail :: Text,
    regPassword :: Text
  }
  deriving (Show, Eq, Generic)

-- Hash a password using SHA256 and encode as Base64
hashPassword :: Text -> Text
hashPassword password =
  let bytes = TE.encodeUtf8 password
      digest = hash bytes :: Digest SHA256
      hashBytes = convert digest :: ByteString
      base64Hash = B64.encode hashBytes
   in TE.decodeUtf8 base64Hash

-- Verify a password against a hash
verifyPassword :: Text -> Text -> Bool
verifyPassword password hashedPassword = hashPassword password == hashedPassword

-- Create a new user in the database
createUser :: Connection -> RegisterData -> IO (Either String Int)
createUser conn regData = do
  let hashedPwd = hashPassword (regPassword regData)
  result <- execute
    conn
    "INSERT INTO users (username, email, password) VALUES (?, ?, ?)"
    (regUsername regData, regEmail regData, hashedPwd)
  lastId <- lastInsertRowId conn
  return $ Right $ fromIntegral lastId

-- Find user by username
findUserByUsername :: Connection -> Text -> IO (Maybe DbUser)
findUserByUsername conn username = do
  users <- query conn "SELECT userID, username, email, password FROM users WHERE username = ?" (Only username)
  case users of
    [user] -> return $ Just user
    _ -> return Nothing

-- Authenticate user with credentials
authenticateUser :: Connection -> LoginCredentials -> IO (Maybe AuthenticatedUser)
authenticateUser conn creds = do
  maybeUser <- findUserByUsername conn (loginUsername creds)
  case maybeUser of
    Nothing -> return Nothing
    Just dbUser ->
      if verifyPassword (loginPassword creds) (dbPassword dbUser)
        then do
          -- For now, return user with empty card collection
          -- In a real app, you'd load their actual collection from DB
          let authUser = User
                { auId = dbUserId dbUser,
                  name = T.unpack $ dbUsername dbUser,
                  email = T.unpack $ dbEmail dbUser,
                  personalCollection = [] :: [Card]
                }
          return $ Just authUser
        else return Nothing

-- Check if username exists
usernameExists :: Connection -> Text -> IO Bool
usernameExists conn username = do
  users <- query conn "SELECT COUNT(*) FROM users WHERE username = ?" (Only username)
  case users of
    [Only count] -> return (count > (0 :: Int))
    _ -> return False

-- Check if email exists  
emailExists :: Connection -> Text -> IO Bool
emailExists conn email = do
  users <- query conn "SELECT COUNT(*) FROM users WHERE email = ?" (Only email)
  case users of
    [Only count] -> return (count > (0 :: Int))
    _ -> return False

-- Validate registration data
validateRegistration :: Connection -> RegisterData -> IO (Either String ())
validateRegistration conn regData = do
  case () of
    _ | T.length (regUsername regData) < 3 -> 
        return $ Left "Username must be at least 3 characters"
      | T.length (regPassword regData) < 6 -> 
        return $ Left "Password must be at least 6 characters"
      | not $ T.isInfixOf "@" (regEmail regData) -> 
        return $ Left "Invalid email format"
      | otherwise -> do
        usernameInUse <- usernameExists conn (regUsername regData)
        if usernameInUse
          then return $ Left "Username already exists"
          else do
            emailInUse <- emailExists conn (regEmail regData)
            if emailInUse
              then return $ Left "Email already exists"
              else return $ Right ()