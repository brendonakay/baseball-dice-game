-- test/DiffTest.hs
--
-- Differential Random Testing (DRT) driver.
--
-- Generates random GameState values, sends them to the Lean reference
-- executable via stdin/stdout JSON, and compares the Lean output with the
-- Haskell implementation's output.  Any divergence is reported as a test
-- failure with the counterexample state.
--
-- The Lean executable is invoked once per transition function, processing
-- all N test inputs in a single subprocess invocation (streaming mode),
-- which avoids per-test subprocess startup cost.
--
-- Tests are skipped (not failed) if the Lean binary is absent.

module DiffTest (runDiffTests) where

import Control.Monad (when)
import Control.Monad.State (runStateT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import System.Process (readProcess)
import Test.Hspec
import Test.QuickCheck
import WaxBall.Game

-- ---------------------------------------------------------------------------
-- Run a Game action from an initial state (pure-ish: modify only)
-- ---------------------------------------------------------------------------

execGame :: GameState -> Game a -> IO GameState
execGame gs action = snd <$> runStateT action gs

-- ---------------------------------------------------------------------------
-- Snapshot: fields the Lean model tracks, used for comparison
-- ---------------------------------------------------------------------------

data Snap = Snap
  { sInning :: Int,
    sHalfInning :: HalfInning,
    sHomeScore :: Int,
    sAwayScore :: Int,
    sOuts :: Int,
    sBalls :: Int,
    sStrikes :: Int
  }
  deriving (Eq)

instance Show Snap where
  show s =
    "{ inning="
      ++ show (sInning s)
      ++ ", half="
      ++ show (sHalfInning s)
      ++ ", home="
      ++ show (sHomeScore s)
      ++ ", away="
      ++ show (sAwayScore s)
      ++ ", outs="
      ++ show (sOuts s)
      ++ ", balls="
      ++ show (sBalls s)
      ++ ", strikes="
      ++ show (sStrikes s)
      ++ " }"

snap :: GameState -> Snap
snap gs =
  Snap
    { sInning = inning gs,
      sHalfInning = halfInning gs,
      sHomeScore = homeScore gs,
      sAwayScore = awayScore gs,
      sOuts = outs gs,
      sBalls = balls gs,
      sStrikes = strikes gs
    }

snapFromLean :: GameState -> Snap
snapFromLean = snap

-- ---------------------------------------------------------------------------
-- Subprocess I/O
-- ---------------------------------------------------------------------------

-- | Encode a list of GameStates as newline-separated JSON (stdin for Lean).
encodeStates :: [GameState] -> String
encodeStates = unlines . map (BLC.unpack . Aeson.encode)

-- | Decode a Lean output line as a GameState (returns Nothing on error).
decodeLine :: String -> Maybe GameState
decodeLine = Aeson.decode . BLC.pack

-- | Invoke the Lean DiffTest binary once for the given command.
--   Returns one Maybe GameState per input state.
callLean :: FilePath -> String -> [GameState] -> IO [Maybe GameState]
callLean binPath cmd states = do
  let input = encodeStates states
  rawOut <- readProcess binPath [cmd] input
  return $ map decodeLine (filter (not . null) (lines rawOut))

-- ---------------------------------------------------------------------------
-- Core DRT check
-- ---------------------------------------------------------------------------

-- | For a given Game action and Lean command, run both on each state and
--   report the first divergence (if any).
runDiffTest :: FilePath -> String -> Game () -> [GameState] -> IO ()
runDiffTest binPath cmd action states = do
  hsResults <- mapM (`execGame` action) states
  leanMaybes <- callLean binPath cmd states
  let triples = zip3 states hsResults leanMaybes
      divergences =
        [ (gs, snap hs, fmap snapFromLean ml)
          | (gs, hs, ml) <- triples,
            case ml of
              Nothing -> True -- parse failure counts as divergence
              Just lr -> snap hs /= snap lr
        ]
  when (not (null divergences)) $ do
    let (gs, hsSnap, leanSnap) = head divergences
    expectationFailure $
      "Divergence for command '"
        ++ cmd
        ++ "':\n"
        ++ "  Input state: "
        ++ show (snap gs)
        ++ "\n"
        ++ "  Haskell:     "
        ++ show hsSnap
        ++ "\n"
        ++ "  Lean:        "
        ++ show leanSnap

-- ---------------------------------------------------------------------------
-- Random state generation (no Arbitrary instances — avoids conflicts)
-- ---------------------------------------------------------------------------

-- Number of states generated per transition.
drtCount :: Int
drtCount = 100

-- | Generate safe-count states: counts below trigger thresholds.
--   balls ∈ {0,1,2}, strikes ∈ {0,1}, outs ∈ {0,1}.
genSafeStates :: IO [GameState]
genSafeStates = generate $ vectorOf drtCount $ do
  inn <- choose (1, 12)
  hi <- elements [Top, Bottom]
  hs <- getNonNegative <$> (arbitrary :: Gen (NonNegative Int))
  as' <- getNonNegative <$> (arbitrary :: Gen (NonNegative Int))
  o <- choose (0, 1)
  b <- choose (0, 2)
  s <- choose (0, 1)
  return $
    newGameState
      { inning = inn,
        halfInning = hi,
        homeScore = hs,
        awayScore = as',
        outs = o,
        balls = b,
        strikes = s
      }

-- ---------------------------------------------------------------------------
-- Test suite
-- ---------------------------------------------------------------------------

runDiffTests :: FilePath -> Spec
runDiffTests binPath = do
  describe "differential (Lean reference)" $ do
    states <- runIO genSafeStates

    let checkCmd descr cmd action =
          it
            ( descr
                ++ ": Lean and Haskell agree on "
                ++ show drtCount
                ++ " random states"
            )
            $ runDiffTest binPath cmd action states

    checkCmd "addBall" "add-ball" addBall
    checkCmd "addStrike" "add-strike" addStrike
    checkCmd "addOut" "add-out" addOut
    checkCmd "clearBalls" "clear-balls" clearBalls
    checkCmd "clearStrikes" "clear-strikes" clearStrikes

    it
      ( "isGameOver: Lean and Haskell agree on "
          ++ show drtCount
          ++ " inning-9 states"
      )
      $ do
        let states9 = map (\gs -> gs {inning = 9}) states
        rawOut <- readProcess binPath ["is-game-over"] (encodeStates states9)
        let outLines = filter (not . null) (lines rawOut)
            hsResults = map isGameOver states9
            parseLean s = case s of
              "true" -> Just True
              "false" -> Just False
              _ -> Nothing
            pairs = zip hsResults (map parseLean outLines)
            divergences =
              [ (hs, ml) | (hs, ml) <- pairs, case ml of Nothing -> True; Just lr -> hs /= lr
              ]
        when (not (null divergences)) $ do
          let (hs, ml) = head divergences
          expectationFailure $
            "isGameOver divergence: Haskell="
              ++ show hs
              ++ " Lean="
              ++ show ml
