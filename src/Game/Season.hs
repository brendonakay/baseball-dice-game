{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- This module contains Season logic for managing a 10-game season.
-- Follows the same patterns as Game.Logic with StateT monad usage.

module Game.Season
  ( SeasonState (..),
    GameResult (..),
    SeasonRef,
    TeamStats (..),
    WinningTeam (..),
    Season,
    newSeasonState,
    startNextGame,
    recordGameResult,
    getCurrentSeasonState,
    isSeasonComplete,
    getSeasonStats,
    runStartNextGame,
    runRecordGameResult,
    advanceCurrentGame,
    runAdvanceCurrentGame,
  )
where

import Control.Monad.State
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef
import GHC.Generics (Generic)
import Game.Logic (AwayTeam, GameState (..), HomeTeam, initialGameState, newGameState)
import qualified Game.State as GS (advanceGameState)

-- State Transformer for Season logic (following Game.Logic pattern)
type Season = StateT SeasonState IO

-- Union type for winning team
data WinningTeam = HomeTeam | AwayTeam
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Result of a completed game
data GameResult = GameResult
  { gameNumber :: Int,
    homeTeamScore :: Int,
    awayTeamScore :: Int,
    winningTeam :: WinningTeam,
    totalInnings :: Int,
    gameState :: GameState -- Store final game state for detailed analysis
  }
  deriving (Show, Eq, Generic)

-- Team statistics for the season
data TeamStats = TeamStats
  { wins :: Int,
    losses :: Int,
    totalRuns :: Int,
    totalRunsAllowed :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- State for a 10-game season
data SeasonState = SeasonState
  { currentGameNumber :: Int, -- 1-10, or 11 if season is complete
    homeTeam :: HomeTeam,
    awayTeam :: AwayTeam,
    gameResults :: [GameResult], -- Completed games (length 0-10)
    homeTeamStats :: TeamStats,
    awayTeamStats :: TeamStats,
    currentGameState :: Maybe GameState -- Current ongoing game (if any)
  }
  deriving (Show, Eq, Generic)

-- Thread-safe reference to season state
type SeasonRef = IORef SeasonState

-- Create initial empty team stats
emptyTeamStats :: TeamStats
emptyTeamStats = TeamStats 0 0 0 0

-- Create a new season state with given teams
newSeasonState :: HomeTeam -> AwayTeam -> SeasonState
newSeasonState homeTeam' awayTeam' =
  SeasonState
    { currentGameNumber = 1,
      homeTeam = homeTeam',
      awayTeam = awayTeam',
      gameResults = [],
      homeTeamStats = emptyTeamStats,
      awayTeamStats = emptyTeamStats,
      currentGameState = Nothing
    }

-- Start the next game in the season (StateT version)
startNextGame :: Season (Maybe GameState)
startNextGame = do
  seasonState <- get
  if isSeasonComplete seasonState
    then return Nothing
    else case currentGameState seasonState of
      Just ongoing -> return (Just ongoing) -- Return ongoing game
      Nothing -> do
        -- Initialize new game state with teams from season
        (_, initializedGameState) <- liftIO $ runStateT (initialGameState (homeTeam seasonState) (awayTeam seasonState)) newGameState
        -- Store the game state in the season
        put $ seasonState {currentGameState = Just initializedGameState}
        return (Just initializedGameState)

-- Record the result of a completed game and update season state (StateT version)
recordGameResult :: GameState -> Season ()
recordGameResult finalGameState = do
  seasonState <- get
  let gameNum = currentGameNumber seasonState
      finalHomeScore = homeScore finalGameState
      finalAwayScore = awayScore finalGameState
      winner = if finalHomeScore > finalAwayScore then HomeTeam else AwayTeam
      innings = inning finalGameState

      result =
        GameResult
          { gameNumber = gameNum,
            homeTeamScore = finalHomeScore,
            awayTeamScore = finalAwayScore,
            winningTeam = winner,
            totalInnings = innings,
            gameState = finalGameState
          }

      -- Update team stats
      newHomeStats =
        if winner == HomeTeam
          then (homeTeamStats seasonState) {wins = wins (homeTeamStats seasonState) + 1, totalRuns = totalRuns (homeTeamStats seasonState) + finalHomeScore, totalRunsAllowed = totalRunsAllowed (homeTeamStats seasonState) + finalAwayScore}
          else (homeTeamStats seasonState) {losses = losses (homeTeamStats seasonState) + 1, totalRuns = totalRuns (homeTeamStats seasonState) + finalHomeScore, totalRunsAllowed = totalRunsAllowed (homeTeamStats seasonState) + finalAwayScore}

      newAwayStats =
        if winner == AwayTeam
          then (awayTeamStats seasonState) {wins = wins (awayTeamStats seasonState) + 1, totalRuns = totalRuns (awayTeamStats seasonState) + finalAwayScore, totalRunsAllowed = totalRunsAllowed (awayTeamStats seasonState) + finalHomeScore}
          else (awayTeamStats seasonState) {losses = losses (awayTeamStats seasonState) + 1, totalRuns = totalRuns (awayTeamStats seasonState) + finalAwayScore, totalRunsAllowed = totalRunsAllowed (awayTeamStats seasonState) + finalHomeScore}

  put $
    seasonState
      { currentGameNumber = gameNum + 1,
        gameResults = result : gameResults seasonState,
        homeTeamStats = newHomeStats,
        awayTeamStats = newAwayStats,
        currentGameState = Nothing -- Clear current game since it's finished
      }

-- Get current season state (read-only)
getCurrentSeasonState :: SeasonRef -> IO SeasonState
getCurrentSeasonState = readIORef

-- Check if season is complete (all 10 games played)
isSeasonComplete :: SeasonState -> Bool
isSeasonComplete seasonState = currentGameNumber seasonState > 10

-- Get season statistics summary
getSeasonStats :: SeasonState -> (TeamStats, TeamStats, Int)
getSeasonStats seasonState =
  ( homeTeamStats seasonState,
    awayTeamStats seasonState,
    length (gameResults seasonState)
  )

-- Wrapper functions for IORef integration (following Game.State pattern)
runStartNextGame :: SeasonRef -> IO (Maybe GameState)
runStartNextGame seasonRef = do
  currentState <- readIORef seasonRef
  (result, newState) <- runStateT startNextGame currentState
  writeIORef seasonRef newState
  return result

runRecordGameResult :: SeasonRef -> GameState -> IO ()
runRecordGameResult seasonRef finalGameState = do
  currentState <- readIORef seasonRef
  (_, newState) <- runStateT (recordGameResult finalGameState) currentState
  writeIORef seasonRef newState

-- Advance the current game by one step (StateT version)
advanceCurrentGame :: Season (Maybe GameState)
advanceCurrentGame = do
  seasonState <- get
  case currentGameState seasonState of
    Nothing -> return Nothing -- No current game
    Just gameState -> do
      -- Advance the game by one step using Game.State functionality
      gameRef <- liftIO $ newIORef gameState
      (_, advancedGameState) <- liftIO $ GS.advanceGameState gameRef
      -- Update the season state with the advanced game
      put $ seasonState {currentGameState = Just advancedGameState}
      return (Just advancedGameState)

-- Wrapper function for IORef integration
runAdvanceCurrentGame :: SeasonRef -> IO (Maybe GameState)
runAdvanceCurrentGame seasonRef = do
  currentState <- readIORef seasonRef
  (result, newState) <- runStateT advanceCurrentGame currentState
  writeIORef seasonRef newState
  return result
