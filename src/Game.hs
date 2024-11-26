module Game where

import Control.Monad (when)
import Control.Monad.State

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
    bases :: BasesState, -- Bases occupied
    currentBatter :: Maybe Player -- Current batter (if any)
  }
  deriving (Show)

-- data BaseState = Maybe Player deriving (Show)
-- I had trouble with this type alias. Was it worth it?

-- TODO: Refactor so we can account for who is on base.
data BasesState = BasesState
  { first :: Maybe Player,
    second :: Maybe Player,
    third :: Maybe Player
  }
  deriving (Show)

-- Example: all bases empty
emptyBases :: BasesState
emptyBases = BasesState Nothing Nothing Nothing

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

advanceRunners :: Game ()
advanceRunners = modify $ \gs ->
  let b = bases gs
      cb = currentBatter gs
      newBases =
        BasesState
          { first = cb,
            second = first b,
            third = second b
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

checkOuts :: Game ()
checkOuts = do
  gs <- get
  when (outs gs >= 3) nextInning

printGameState :: GameState -> IO ()
printGameState gs =
  print (show gs)
