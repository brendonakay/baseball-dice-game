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

-- TODO: Break homeScore and awayScore out to a generic Score type that is a
-- record of runs, hits, and errors
data GameState = GameState
  { inning :: Int, -- Current inning
    homeScore :: Int, -- Home team's score
    awayScore :: Int, -- Away team's score
    outs :: Int, -- Number of outs in the inning
    balls :: Int, -- Number of balls in the inning
    bases :: BasesState, -- Bases occupied
    currentBatter :: Maybe Player -- Current batter (if any)
  }
  deriving (Show)

type Team = [Player]

newtype HomeTeam = HomeTeam Team

newtype AwayTeam = AwayTeam Team

-- data BaseState = Maybe Player deriving (Show)
-- I had trouble with this type alias. Was it worth it?

-- TODO: Refactor so we can account for who is on base.
data BasesState = BasesState
  { first :: Maybe Player,
    second :: Maybe Player,
    third :: Maybe Player,
    home :: Maybe Player
  }
  deriving (Show)

-- Example: all bases empty
emptyBases :: BasesState
emptyBases = BasesState Nothing Nothing Nothing Nothing

-- Initial game state
initialGameState :: GameState
initialGameState =
  GameState
    { inning = 1,
      homeScore = 0,
      awayScore = 0,
      outs = 0,
      balls = 0,
      bases = emptyBases,
      currentBatter = Nothing
    }

addRun :: Bool -> Game ()
addRun isHomeTeam = modify $ \gs ->
  if isHomeTeam
    then gs {homeScore = homeScore gs + 1}
    else gs {awayScore = awayScore gs + 1}

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

addOut :: Game ()
addOut = modify $ \gs -> gs {outs = outs gs + 1}

addBall :: Game ()
addBall = modify $ \gs -> gs {balls = balls gs + 1}

batterUp :: Game ()
batterUp =
  modify $
    \gs -> gs {currentBatter = Just Player {name = "A", number = 01}}

nextInning :: Game ()
nextInning = modify $ \gs ->
  gs
    { inning = inning gs + 1,
      outs = 0,
      bases = emptyBases
    }

checkOuts :: Game ()
checkOuts = do
  gs <- get
  when (outs gs >= 3) nextInning

checkScore :: Game ()
checkScore = do
  gs <- get
  when (isJust (home (bases gs))) $
    modify $
      \gs' -> gs' {homeScore = homeScore gs' + 1}

runStrikeAction :: Int -> Int -> Game ()
runStrikeAction a b = do
  let strikeAction = getStrikeAction a b
  case strikeAction of
    FieldingError -> runFieldingError
    FlyOut -> runFlyOut
    GroundOut -> runGroundOut
    HitByPitch -> runHitByPitch
    HitDouble -> runHitDouble
    HitSingle -> runHitSingle
    HitTriple -> runHitTriple
    HomeRun -> runHomeRun
    PopOut -> runPopOut
    CalledStrike -> runCalledStrike
    NoAction -> pure ()

runPitch :: Pitch -> Int -> Int -> Game ()
runPitch p a b = do
  batterUp
  case p of
    Ball -> addBall
    Strike -> runStrikeAction a b

runCalledStrike :: Game ()
runCalledStrike = addOut

-- Runners advance 1 base
runFieldingError :: Game ()
runFieldingError = advanceRunners

runFlyOut :: Game () -- TODO: Add sac fly
runFlyOut = addOut

runGroundOut :: Game ()
runGroundOut = addOut -- TODO: Add doubleplay

runHitByPitch :: Game ()
runHitByPitch = advanceRunners

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

-- TODO: Next batter game state changer
-- nextBatter :: Game ()
-- nextBatter =
