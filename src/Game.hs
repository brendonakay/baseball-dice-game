module Game where

-- TODO: Move this to its own folder and submodules. E.G. Logic for pure
-- functions, "State" for game effects? IDK. Maybe a better name for that.

import Control.Monad (when)
import Control.Monad.State
import Data.Maybe (isJust)
import Rules
  ( StrikeAction (..),
    getStrikeAction,
  )

data Pitch = Ball | Strike
  deriving (Show)

type Game = State GameState

data Player = Player
  { name :: String,
    number :: Int
  }
  deriving (Show)

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
    homeBatting_ :: Int,
    awayBatting_ :: Int,
    homeScore_ :: Int,
    awayScore_ :: Int,
    balls_ :: Int,
    strikes_ :: Int,
    bases_ :: BasesState
  }
  deriving (Show)

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
            bases_ = bases gs
          }
  modify $ \gs' -> gs' {pitchLog = log' : pitchLog gs}

data HalfInning = Bottom | Top deriving (Show, Eq)

-- TODO: Break homeScore and awayScore out to a generic Score type that is a
-- record of runs, hits, and errors
data GameState = GameState
  { inning :: Int, -- Current inning
    halfInning :: HalfInning, -- Top or Bottom of the inning
    homeBatting :: Int, -- Batting order, simple mod 9
    awayBatting :: Int,
    homeScore :: Int, -- Home team's score
    awayScore :: Int, -- Away team's score
    outs :: Int, -- Number of outs in the inning
    balls :: Int, -- Number of balls in the inning
    strikes :: Int, -- Number of strikes in the inning
    bases :: BasesState, -- Bases occupied
    currentBatter :: Maybe Player, -- Current batter (if any)
    pitchLog :: PitchLog -- Log of pitch actions
  }
  deriving (Show)

type Team = [Player]

type HomeTeam = Team

type AwayTeam = Team

-- TODO: Refactor so we can account for who is on base.
data BasesState = BasesState
  { first :: Maybe Player,
    second :: Maybe Player,
    third :: Maybe Player,
    home :: Maybe Player
  }
  deriving (Show)

emptyBases :: BasesState
emptyBases = BasesState Nothing Nothing Nothing Nothing

-- Initial game state
initialGameState :: GameState
initialGameState =
  GameState
    { inning = 1,
      halfInning = Top,
      homeBatting = 0,
      awayBatting = 0,
      homeScore = 0,
      awayScore = 0,
      outs = 0,
      balls = 0,
      strikes = 0,
      bases = emptyBases,
      currentBatter = Nothing,
      pitchLog =
        [ Log
            { currentBatter_ = Nothing,
              strikeAction_ = NoAction,
              inning_ = 0,
              halfInning_ = Top,
              homeBatting_ = 0,
              awayBatting_ = 0,
              homeScore_ = 0,
              awayScore_ = 0,
              balls_ = 0,
              strikes_ = 0,
              bases_ = emptyBases
            }
        ]
    }

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
     in gs {bases = newBases, currentBatter = Nothing}
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
    Just Player {} -> advanceRunners
    Nothing -> runnerToFirst

addOut :: Game ()
addOut = do
  modify $ \gs -> gs {outs = outs gs + 1}
  checkOuts

addBall :: Game ()
addBall = do
  checkBalls
  modify $ \gs -> gs {balls = balls gs + 1}

addStrike :: Game ()
addStrike = do
  modify $ \gs -> gs {strikes = strikes gs + 1}
  checkStrikes

clearBalls :: Game ()
clearBalls = modify $ \gs -> gs {balls = 0}

clearStrikes :: Game ()
clearStrikes = modify $ \gs -> gs {strikes = 0}

batterUp :: HomeTeam -> AwayTeam -> Game ()
batterUp ht at = do
  gs <- get
  case halfInning gs of
    Top -> modify $
      \gs' ->
        gs'
          { currentBatter = Just (at !! awayBatting gs),
            awayBatting = mod (awayBatting gs + 1) 9
          }
    Bottom -> modify $
      \gs' ->
        gs'
          { currentBatter = Just (ht !! homeBatting gs),
            homeBatting = mod (homeBatting gs + 1) 9
          }

nextHalfInning :: Game ()
nextHalfInning = do
  gs <- get
  case halfInning gs of
    Top -> modify $ \gs' ->
      gs'
        { outs = 0,
          bases = emptyBases,
          halfInning = Bottom
        }
    Bottom -> modify $ \gs' ->
      gs'
        { inning = inning gs' + 1,
          outs = 0,
          bases = emptyBases,
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

runPitch :: HomeTeam -> AwayTeam -> Pitch -> Int -> Int -> Game StrikeAction
runPitch ht at p a b = do
  batterUp ht at
  case p of
    Ball -> do
      addBall
      pure NoAction
    Strike -> do
      -- TODO: How do I compose these?
      s <- runStrikeAction a b
      logPitchGameState s
      pure s

runCalledStrike :: Game ()
runCalledStrike = addStrike

-- Runners advance 1 base
runFieldingError :: Game ()
runFieldingError = advanceRunners

runFlyOut :: Game () -- TODO: Add sac fly
runFlyOut = addOut

runGroundOut :: Game ()
runGroundOut = addOut -- TODO: Add doubleplay

runHitByPitch :: Game ()
runHitByPitch = walkRunner

runHitDouble :: Game ()
runHitDouble = do
  advanceRunners
  advanceRunners

runHitSingle :: Game ()
runHitSingle = do
  advanceRunners

runHitTriple :: Game ()
runHitTriple = do
  advanceRunners
  advanceRunners
  advanceRunners

runHomeRun :: Game ()
runHomeRun = do
  advanceRunners
  advanceRunners
  advanceRunners
  advanceRunners

runPopOut :: Game ()
runPopOut = addOut
