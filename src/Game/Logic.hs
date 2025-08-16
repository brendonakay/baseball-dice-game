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
    StrikeAction (..),
    HalfInning (..),
    BasesState (..),
    Log (..),
  )
where

import Control.Monad (replicateM_, when)
import Control.Monad.State
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (catMaybes, isJust)
import GHC.Generics (Generic)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)

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
    number :: Int,
    battingAverage :: Double,
    onBasePercentage :: Double,
    sluggingPercentage :: Double
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
  gs <- get
  case currentBatter gs of
    Nothing -> pure NoAction -- Do nothing if no current batter
    Just player -> do
      -- Use new batting average system
      strikeAction <- liftIO $ getPlayerStrikeAction player a b
      executeStrikeAction strikeAction

executeStrikeAction :: StrikeAction -> Game StrikeAction
executeStrikeAction strikeAction = do
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
  gs <- get
  let cb = currentBatter gs
      b = bases gs
      newBases = BasesState
        { first = Nothing,
          second = cb,
          third = first b,
          home = second b
        }
  modify $ \gs' -> gs' {bases = newBases}
  checkScore
  clearStrikes
  clearBalls
  batterUp

runHitSingle :: Game ()
runHitSingle = do
  advanceRunners
  batterUp

runHitTriple :: Game ()
runHitTriple = do
  gs <- get
  let cb = currentBatter gs
      b = bases gs
      newBases = BasesState
        { first = Nothing,
          second = Nothing,
          third = cb,
          home = first b
        }
  modify $ \gs' -> gs' {bases = newBases}
  checkScore
  clearStrikes
  clearBalls
  batterUp

runHomeRun :: Game ()
runHomeRun = do
  gs <- get
  let cb = currentBatter gs
      b = bases gs
      newBases = BasesState
        { first = Nothing,
          second = Nothing,
          third = Nothing,
          home = Nothing
        }
      runCount = 1 + length (catMaybes [first b, second b, third b])
      hi = halfInning gs
  modify $ \gs' -> gs' {bases = newBases}
  replicateM_ runCount (addRun hi)
  clearStrikes
  clearBalls
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

-- New batting average-based hit determination
getPlayerStrikeAction :: Player -> Int -> Int -> IO StrikeAction
getPlayerStrikeAction player dice1 dice2 = do
  hitRoll <- randomRIO (0.0, 1.0) :: IO Double
  let diceModifier = fromIntegral (dice1 + dice2) / 12.0 -- Normalize dice to 0.17-1.0 range
      adjustedAverage = battingAverage player * diceModifier

  if hitRoll <= adjustedAverage
    then determineHitType player dice1 dice2
    else determineOutType dice1 dice2

-- Determine type of hit based on player's slugging percentage and dice
determineHitType :: Player -> Int -> Int -> IO StrikeAction
determineHitType player dice1 dice2 = do
  hitTypeRoll <- randomRIO (0.0, 1.0) :: IO Double
  let powerFactor = sluggingPercentage player
      diceSum = dice1 + dice2

  -- Hit type distribution based on realistic baseball statistics:
  -- Singles: 70% of hits, Doubles: 20% of hits, Triples: 5% of hits, Home Runs: 5% of hits
  -- Adjusted by player's slugging percentage for power hitters
  let singleThreshold = 0.70 - (powerFactor - 0.4) * 0.2 -- Higher power = fewer singles
      doubleThreshold = singleThreshold + 0.20 + (powerFactor - 0.4) * 0.1
      tripleThreshold = doubleThreshold + 0.05 + (if diceSum >= 10 then 0.02 else 0.0)

  if hitTypeRoll <= singleThreshold
    then return HitSingle
    else
      if hitTypeRoll <= doubleThreshold
        then return HitDouble
        else
          if hitTypeRoll <= tripleThreshold
            then return HitTriple
            else return HomeRun

-- Determine type of out based on dice
determineOutType :: Int -> Int -> IO StrikeAction
determineOutType dice1 dice2 = do
  outTypeRoll <- randomRIO (0.0, 1.0) :: IO Double

  -- Special actions based on dice combinations (preserving some original logic)
  case (dice1, dice2) of
    (1, 3) -> return HitByPitch -- Keep hit by pitch on specific dice
    (4, 4) -> return FieldingError -- Keep fielding error on double 4s
    _ ->
      if outTypeRoll <= 0.4
        then return GroundOut
        else
          if outTypeRoll <= 0.7
            then return FlyOut
            else
              if outTypeRoll <= 0.9
                then return PopOut
                else return CalledStrike

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
