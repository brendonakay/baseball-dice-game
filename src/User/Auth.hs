{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module User.Auth where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple
import GHC.Generics
import Servant.Auth.Server (AuthResult (..), BasicAuthData (..), FromBasicAuthData (..))
import User.AuthenticatedUser (AuthenticatedUser (..))
import WaxBall.Card (Card (..), Class (..), Special (..), Type (..))
import WaxBall.Game (Player)
import qualified WaxBall.Game as Game

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

-- FromRow instance for Player
instance FromRow Player where
  fromRow = Game.Player <$> field <*> field <*> field <*> field <*> field

-- Helper function to parse Special from database text
parseSpecial :: Maybe Text -> Maybe Special
parseSpecial Nothing = Nothing
parseSpecial (Just "AUTOGRAPH") = Just Autograph
parseSpecial (Just "SERIAL") = Just Serial
parseSpecial _ = Nothing

-- Helper function to parse Class from database text
parseClass :: Text -> Class
parseClass "BASE" = Base
parseClass "INSERT" = Insert
parseClass "PARALLEL" = Parallel
parseClass _ = Base -- Default fallback

-- Database representation for Card with flattened Player data
data DbCard = DbCard
  { dbCardId :: Int,
    dbCardNumber :: Text,
    dbCardTeam :: Text,
    dbCardClass :: Text,
    dbCardSpecial :: Maybe Text,
    -- Player fields
    dbPlayerName :: Text,
    dbPlayerNumber :: Int,
    dbPlayerBattingAverage :: Double,
    dbPlayerOnBasePercentage :: Double,
    dbPlayerSluggingPercentage :: Double
  }
  deriving (Show, Eq, Generic)

instance FromRow DbCard where
  fromRow =
    DbCard
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

-- Convert DbCard to Card
dbCardToCard :: DbCard -> Card
dbCardToCard dbCard =
  Card
    { WaxBall.Card.id = dbCardId dbCard,
      WaxBall.Card.number = T.unpack $ dbCardNumber dbCard,
      player =
        Game.Player
          { Game.name = T.unpack $ dbPlayerName dbCard,
            Game.number = dbPlayerNumber dbCard,
            Game.battingAverage = dbPlayerBattingAverage dbCard,
            Game.onBasePercentage = dbPlayerOnBasePercentage dbCard,
            Game.sluggingPercentage = dbPlayerSluggingPercentage dbCard
          },
      team = T.unpack $ dbCardTeam dbCard,
      cardType = Type (parseClass $ dbCardClass dbCard) (parseSpecial $ dbCardSpecial dbCard)
    }

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
  _ <-
    execute
      conn
      "INSERT INTO users (username, email, password) VALUES (?, ?, ?)"
      (regUsername regData, regEmail regData, hashedPwd)
  lastId <- lastInsertRowId conn
  return $ Right $ fromIntegral lastId

-- Find user by username
findUserByUsername :: Connection -> Text -> IO (Maybe DbUser)
findUserByUsername conn username = do
  users <- query conn "SELECT id, username, email, password FROM users WHERE username = ?" (Only username)
  case users of
    [user] -> return $ Just user
    _ -> return Nothing

-- Fetch user's personal collection of cards
fetchUserCards :: Connection -> Int -> IO [Card]
fetchUserCards conn userId = do
  dbCards <-
    query
      conn
      "SELECT c.id, c.number, c.team, c.card_class, c.special, \
      \       p.name, p.number, p.batting_average, p.on_base_percentage, p.slugging_percentage \
      \FROM cards c \
      \JOIN players p ON c.player_id = p.id \
      \WHERE c.user_id = ?"
      (Only userId)
  return $ map dbCardToCard dbCards

-- Authenticate user with credentials
authenticateUser :: Connection -> LoginCredentials -> IO (Maybe AuthenticatedUser)
authenticateUser conn creds = do
  maybeUser <- findUserByUsername conn (loginUsername creds)
  case maybeUser of
    Nothing -> return Nothing
    Just dbUser ->
      if verifyPassword (loginPassword creds) (dbPassword dbUser)
        then do
          -- Create user without loading cards - cards will be loaded when needed
          let authUser =
                User
                  { auId = dbUserId dbUser,
                    User.AuthenticatedUser.name = T.unpack $ dbUsername dbUser,
                    User.AuthenticatedUser.email = T.unpack $ dbEmail dbUser,
                    personalCollection = [] -- Empty collection, will be loaded separately
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
-- TODO: This whole damn thing needs to be way more robust
validateRegistration :: Connection -> RegisterData -> IO (Either String ())
validateRegistration conn regData = do
  case () of
    _
      | T.length (regUsername regData) < 3 ->
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

-- FromBasicAuthData instance for AuthenticatedUser
-- This is a dummy instance since we use AuthCheck in Main.hs with database access
instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData _ _ = return NoSuchUser -- This will never be called due to AuthCheck

-- Authentication check function for servant-auth
-- This function will be used to create the authentication context
authCheck :: Connection -> BasicAuthData -> IO (Maybe AuthenticatedUser)
authCheck conn (BasicAuthData username password) = do
  let creds = LoginCredentials (TE.decodeUtf8 username) (TE.decodeUtf8 password)
  authenticateUser conn creds
