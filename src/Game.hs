module Game where

import Control.Monad (when)
import Control.Monad.State

data Pitch = Ball | Strike
  deriving (Show)

data Base = Home | First | Second | Third deriving (Show, Eq, Enum)

type Position = (Double, Double)

type Game = State GameState

basePositions :: [(Base, Position)]
basePositions =
  [ (Home, (0, 0)),
    (First, (90, 0)), -- Assuming 90 feet between bases
    (Second, (90, 90)),
    (Third, (0, 90))
  ]

data Diamond = Diamond
  { homePlate :: Position,
    firstBase :: Position,
    secondBase :: Position,
    thirdBase :: Position
  }
  deriving (Show)

defaultDiamond :: Diamond
defaultDiamond =
  Diamond
    { homePlate = (0, 0),
      firstBase = (90, 0),
      secondBase = (90, 90),
      thirdBase = (0, 90)
    }

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
    bases :: BasesOccupied, -- Bases occupied
    currentBatter :: Maybe Player -- Current batter (if any)
  }
  deriving (Show)

data BasesOccupied = BasesOccupied
  { firstOccupied :: Bool,
    secondOccupied :: Bool,
    thirdOccupied :: Bool
  }
  deriving (Show)

-- Example: all bases empty
emptyBases :: BasesOccupied
emptyBases = BasesOccupied False False False

-- Initial game state
initialGameState :: GameState
initialGameState =
  GameState
    { inning = 1,
      homeScore = 0,
      awayScore = 0,
      outs = 0,
      bases = emptyBases,
      currentBatter = Nothing
    }

addRun :: Bool -> Game ()
addRun isHomeTeam = modify $ \gs ->
  if isHomeTeam
    then gs {homeScore = homeScore gs + 1}
    else gs {awayScore = awayScore gs + 1}

advanceRunners :: Int -> Game ()
advanceRunners n = modify $ \gs ->
  let b = bases gs
      newBases =
        BasesOccupied
          { firstOccupied = n > 0 && not (secondOccupied b),
            secondOccupied = firstOccupied b,
            thirdOccupied = secondOccupied b
          }
   in gs {bases = newBases}

addOut :: Game ()
addOut = modify $ \gs -> gs {outs = outs gs + 1}

nextInning :: Game ()
nextInning = modify $ \gs ->
  gs
    { inning = inning gs + 1,
      outs = 0,
      bases = emptyBases
    }

batterToFirst :: Game ()
batterToFirst = do
  advanceRunners 1
  modify $ \gs -> gs {currentBatter = Nothing}

checkOuts :: Game ()
checkOuts = do
  gs <- get
  when (outs gs >= 3) nextInning

printGameState :: GameState -> IO ()
printGameState gs =
  print (show gs)
