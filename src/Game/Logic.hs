{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- This module contains the primary game logic.
-- Run functions, prefixed with "run", are used in Control.Monad.State
-- runState functions.

module Game.Logic
  ( initialGameState,
    logFields,
    newGameState,
    pitchBallOrStrike,
    pitchLogToString,
    runPitch,
    testStateChange,
    AwayTeam,
    GameState (..),
    HomeTeam,
    Player (..),
  )
where

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import System.IO (hFlush, stdout)

data Pitch = Ball | Strike
  deriving (Show)

-- State Transformer for Game logic
type Game = StateT GameState IO

-- Helper function to create a debugging state transformer
debugStateChange :: String -> Game a -> Game a
debugStateChange description action = do
  oldState <- get
  result <- action
  newState <- get
  when (oldState /= newState) $ do
    liftIO $ do
      putStrLn $ "State Change: " ++ description
      putStrLn $ "  Previous state: " ++ show oldState
      putStrLn $ "  New state: " ++ show newState
      hFlush stdout
  return result

data Player = Player
  { name :: String,
    number :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

pitchBallOrStrike :: Int -> Pitch
pitchBallOrStrike n =
  if even n
    then Ball
    else Strike

-- TODO: How do I prevent naming collisions? Move to different modules?
data Log = Log
  { currentBatter_ :: Maybe Player,
    strikeAction_ :: StrikeAction,
    inning_ :: Int,
    halfInning_ :: HalfInning,
    homeBatting_ :: [Player],
    awayBatting_ :: [Player],
    homeScore_ :: Int,
    awayScore_ :: Int,
    balls_ :: Int,
    strikes_ :: Int,
    outs_ :: Int,
    bases_ :: BasesState
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- It was just easier to keep a static list.
-- Maybe there's a better way to dynamically derive the list of field names
-- for this record.
logFields :: [String]
logFields =
  [ "Current Batter",
    "Strike Action",
    "Inning",
    "Half Inning",
    "Home Batting",
    "Away Batting",
    "Home Score",
    "Away Score",
    "Balls",
    "Strikes",
    "Bases"
  ]

type PitchLog = [Log]

logPitchGameState :: StrikeAction -> Game ()
logPitchGameState s = do
  gs <- get
  let log' =
        Log
          { currentBatter_ = currentBatter gs,
            strikeAction_ = s,
            inning_ = inning gs,
            halfInning_ = halfInning gs,
            homeBatting_ = homeBatting gs,
            awayBatting_ = awayBatting gs,
            homeScore_ = homeScore gs,
            awayScore_ = awayScore gs,
            balls_ = balls gs,
            strikes_ = strikes gs,
            outs_ = outs gs,
            bases_ = bases gs
          }
  modify $ \gs' -> gs' {pitchLog = log' : pitchLog gs}

data HalfInning
  = Bottom
  | Top
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- TODO: Break homeScore and awayScore out to a generic Score type that is a
-- record of runs, hits, and errors
data GameState = GameState
  { inning :: Int, -- Current inning.
    halfInning :: HalfInning, -- Top or Bottom of the inning.
    homeBatting :: [Player],
    awayBatting :: [Player],
    homeScore :: Int, -- Home team's score.
    awayScore :: Int, -- Away team's score.
    outs :: Int, -- Number of outs in the inning.
    balls :: Int, -- Number of balls in the inning.
    strikes :: Int, -- Number of strikes in the inning.
    bases :: BasesState, -- Bases occupied.
    currentBatter :: Maybe Player, -- Current batter (if any).
    pitchLog :: PitchLog -- Log of pitch actions.
  }
  deriving (Show, Eq)

type Team = [Player]

type HomeTeam = Team

type AwayTeam = Team

data BasesState = BasesState
  { first :: Maybe Player,
    second :: Maybe Player,
    third :: Maybe Player,
    home :: Maybe Player
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

emptyBases :: BasesState
emptyBases = BasesState Nothing Nothing Nothing Nothing

newGameState :: GameState
newGameState =
  GameState
    { inning = 1,
      halfInning = Top,
      homeBatting = [],
      awayBatting = [],
      homeScore = 0,
      awayScore = 0,
      outs = 0,
      balls = 0,
      strikes = 0,
      bases = emptyBases,
      currentBatter = Nothing,
      pitchLog =
        []
    }

-- Initial game state. Set the batting order for both teams and bring the first
-- player to the plate.
initialGameState :: HomeTeam -> AwayTeam -> Game ()
initialGameState ht at = do
  modify $ \gs ->
    gs
      { homeBatting = ht,
        awayBatting = at
      }
  batterUp

addRun :: HalfInning -> Game ()
addRun hi = modify $ \gs ->
  case hi of
    Top -> gs {awayScore = awayScore gs + 1}
    Bottom -> gs {homeScore = homeScore gs + 1}

advanceRunners :: Game ()
advanceRunners = do
  modify $ \gs ->
    let b = bases gs
        cb = currentBatter gs
        newBases =
          BasesState
            { first = cb,
              second = first b,
              third = second b,
              home = third b
            }
     in gs {bases = newBases}
  checkScore
  clearStrikes
  clearBalls

runnerToFirst :: Game ()
runnerToFirst = do
  modify $ \gs ->
    let b = bases gs
        cb = currentBatter gs
        newBases =
          BasesState
            { first = cb,
              second = second b,
              third = third b,
              home = Nothing
            }
     in gs {bases = newBases, currentBatter = Nothing}
  clearStrikes
  clearBalls

walkRunner :: Game ()
walkRunner = do
  gs <- get
  case first (bases gs) of
    Just _ -> do
      advanceRunners
      batterUp
    Nothing -> do
      runnerToFirst
      batterUp

addOut :: Game ()
addOut = do
  modify $ \gs -> gs {outs = outs gs + 1}
  checkOuts

addBall :: Game ()
addBall = do
  modify $ \gs -> gs {balls = balls gs + 1}
  checkBalls

addStrike :: Game ()
addStrike = debugStateChange "Adding a strike" $ do
  modify $ \gs -> gs {strikes = strikes gs + 1}
  checkStrikes

clearBalls :: Game ()
clearBalls = modify $ \gs -> gs {balls = 0}

clearStrikes :: Game ()
clearStrikes = modify $ \gs -> gs {strikes = 0}

batterUp :: Game ()
batterUp = do
  gs <- get
  case halfInning gs of
    Top -> do
      modify $
        \gs' ->
          gs'
            { currentBatter = Just (head (awayBatting gs))
            }
      cycleBattingOrder
    Bottom -> do
      modify $
        \gs' ->
          gs'
            { currentBatter = Just (head (homeBatting gs))
            }
      cycleBattingOrder

cycleBattingOrder :: Game ()
cycleBattingOrder = do
  gs <- get
  case halfInning gs of
    Top -> modify $
      \gs' ->
        gs'
          { awayBatting =
              [ awayBatting gs !! (i `mod` length (awayBatting gs))
                | i <- [1 .. length (awayBatting gs)]
              ]
          }
    Bottom -> modify $
      \gs' ->
        gs'
          { homeBatting =
              [ homeBatting gs !! (i `mod` length (homeBatting gs))
                | i <- [1 .. length (homeBatting gs)]
              ]
          }

nextHalfInning :: Game ()
nextHalfInning = do
  gs <- get
  case halfInning gs of
    Top -> modify $ \gs' ->
      gs'
        { outs = 0,
          balls = 0,
          bases = emptyBases,
          currentBatter = Nothing,
          halfInning = Bottom
        }
    Bottom -> modify $ \gs' ->
      gs'
        { inning = inning gs' + 1,
          outs = 0,
          balls = 0,
          bases = emptyBases,
          currentBatter = Nothing,
          halfInning = Top
        }

checkOuts :: Game ()
checkOuts = do
  gs <- get
  when (outs gs == 3) nextHalfInning

checkScore :: Game ()
checkScore = do
  gs <- get
  let hi = halfInning gs
  when (isJust (home (bases gs))) $
    addRun hi

checkBalls :: Game ()
checkBalls = do
  gs <- get
  when (balls gs == 4) walkRunner

checkStrikes :: Game ()
checkStrikes = do
  gs <- get
  when (strikes gs == 3) addOut

runStrikeAction :: Int -> Int -> Game StrikeAction
runStrikeAction a b = do
  let strikeAction = getStrikeAction a b
  case strikeAction of
    FieldingError -> do
      runFieldingError
      pure FieldingError
    FlyOut -> do
      runFlyOut
      pure FlyOut
    GroundOut -> do
      runGroundOut
      pure GroundOut
    HitByPitch -> do
      runHitByPitch
      pure HitByPitch
    HitDouble -> do
      runHitDouble
      pure HitDouble
    HitSingle -> do
      runHitSingle
      pure HitSingle
    HitTriple -> do
      runHitTriple
      pure HitTriple
    HomeRun -> do
      runHomeRun
      pure HomeRun
    PopOut -> do
      runPopOut
      pure PopOut
    CalledStrike -> do
      runCalledStrike
      pure CalledStrike
    NoAction -> pure NoAction

runPitch ::
  Pitch -> -- Diceroll for the pitch.
  Int -> -- Diceroll for a strike.
  Int ->
  Game StrikeAction
runPitch p a b = do
  case p of
    Ball -> do
      addBall
      -- NoAction will stand for a ball.
      -- I should look into a better abstraction.
      logPitchGameState NoAction
      pure NoAction
    Strike -> do
      s <- runStrikeAction a b
      logPitchGameState s
      pure s

runCalledStrike :: Game ()
runCalledStrike = addStrike

-- Runners advance 1 base
runFieldingError :: Game ()
runFieldingError = do
  advanceRunners
  batterUp

runFlyOut :: Game () -- TODO: Add sac fly
runFlyOut = do
  addOut
  batterUp

runGroundOut :: Game ()
runGroundOut = do
  addOut -- TODO: Add doubleplay
  batterUp

runHitByPitch :: Game ()
runHitByPitch = walkRunner

runHitDouble :: Game ()
runHitDouble = do
  advanceRunners
  advanceRunners
  batterUp

runHitSingle :: Game ()
runHitSingle = do
  advanceRunners
  batterUp

runHitTriple :: Game ()
runHitTriple = do
  advanceRunners
  advanceRunners
  advanceRunners
  batterUp

runHomeRun :: Game ()
runHomeRun = do
  advanceRunners
  advanceRunners
  advanceRunners
  advanceRunners
  batterUp

runPopOut :: Game ()
runPopOut = do
  addOut
  batterUp

data StrikeAction
  = FieldingError
  | FlyOut
  | GroundOut
  | HitByPitch
  | HitDouble
  | HitSingle
  | HitTriple
  | HomeRun
  | PopOut
  | CalledStrike
  | NoAction
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

getStrikeAction :: Int -> Int -> StrikeAction
getStrikeAction a b =
  case (a, b) of
    (1, 1) -> HitDouble
    (1, 2) -> GroundOut
    (1, 3) -> HitByPitch
    (1, 4) -> HitSingle
    (1, 5) -> GroundOut
    (1, 6) -> CalledStrike
    (2, 2) -> HitDouble
    (2, 3) -> PopOut
    (2, 4) -> HitSingle
    (2, 5) -> CalledStrike
    (2, 6) -> GroundOut
    (3, 3) -> HitTriple
    (3, 4) -> CalledStrike
    (3, 5) -> GroundOut
    (3, 6) -> FlyOut
    (4, 4) -> FieldingError
    (4, 5) -> FlyOut
    (4, 6) -> FlyOut
    (5, 5) -> HitSingle
    (5, 6) -> PopOut
    (6, 6) -> HomeRun
    (_, _) -> NoAction

pitchLogToString :: PitchLog -> [[String]]
pitchLogToString = map pitchLogToStringList

pitchLogToStringList :: Log -> [String]
pitchLogToStringList (Log cb sa i hi hb ab hs as b s o ba) =
  [ show cb,
    show sa,
    show i,
    show hi,
    show hb,
    show ab,
    show hs,
    show as,
    show b,
    show s,
    show o,
    show ba
  ]

-- Test function to demonstrate state change debugging
testStateChange :: Game ()
testStateChange = do
  liftIO $ putStrLn "Testing state change debugging..."
  addStrike
  addStrike
  liftIO $ putStrLn "Test completed!"
