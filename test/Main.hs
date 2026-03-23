-- test/Main.hs
--
-- Property-based tests bridging the Lean formal specification to the Haskell
-- implementation. Each QuickCheck property here corresponds to a theorem
-- proved in lean/BaseballVerify/Proofs/.
--
-- The spec gap: Lean proofs verify properties of the Lean *model*. These tests
-- verify the same properties hold on the actual Haskell *implementation*, so
-- that a divergence between model and code is caught at test time.

module Main where

import Control.Monad (when)
import Control.Monad.State (runStateT)
import DiffTest (runDiffTests)
import System.Directory (doesFileExist)
import Test.Hspec
import Test.QuickCheck
import WaxBall.Game

-- | Run a Game action from the given state, returning only the resulting state.
execGame :: GameState -> Game a -> IO GameState
execGame gs action = snd <$> runStateT action gs

-- Arbitrary instances ---------------------------------------------------------

instance Arbitrary HalfInning where
  arbitrary = elements [Top, Bottom]

instance Arbitrary Player where
  arbitrary =
    Player
      <$> arbitrary
      <*> arbitrary
      <*> choose (0.150, 0.400)
      <*> choose (0.200, 0.500)
      <*> choose (0.300, 0.700)

instance Arbitrary BasesState where
  arbitrary =
    BasesState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

-- | A GameState with count fields below their check-trigger thresholds:
--   balls ∈ {0,1,2}   — addBall will not trigger a walk (fires at 4)
--   strikes ∈ {0,1}   — addStrike will not trigger a strikeout (fires at 3)
--   outs ∈ {0,1}      — addOut will not trigger a half-inning change (fires at 3)
--
-- This lets us test the pure increment invariants in isolation, mirroring
-- the pure-core functions in the Lean model.
newtype SafeCountState = SafeCountState GameState
  deriving (Show)

instance Arbitrary SafeCountState where
  arbitrary = do
    inn <- choose (1, 12)
    hi <- arbitrary
    hs <- getNonNegative <$> arbitrary
    as' <- getNonNegative <$> arbitrary
    o <- choose (0, 1)
    b <- choose (0, 2)
    s <- choose (0, 1)
    return $
      SafeCountState $
        newGameState
          { inning = inn,
            halfInning = hi,
            homeScore = hs,
            awayScore = as',
            outs = o,
            balls = b,
            strikes = s
          }

-- Tests -----------------------------------------------------------------------

-- Path to the Lean DiffTest oracle.
-- Build with: nix build .#difftest  (produces result/bin/DiffTest)
leanBin :: FilePath
leanBin = "result/bin/DiffTest"

main :: IO ()
main = hspec $ do
  -- -------------------------------------------------------------------------
  -- addBall invariants
  -- Mirrors: CountInvariants.addBall_increments, addBall_preserves_outs, etc.
  -- -------------------------------------------------------------------------
  describe "addBall (balls ≤ 2, no walk triggered)" $ do
    it "increments balls by 1" $
      property $
        \(SafeCountState gs) -> ioProperty $ do
          gs' <- execGame gs addBall
          return $ balls gs' == balls gs + 1

    it "preserves outs" $
      property $
        \(SafeCountState gs) -> ioProperty $ do
          gs' <- execGame gs addBall
          return $ outs gs' == outs gs

    it "preserves strikes" $
      property $
        \(SafeCountState gs) -> ioProperty $ do
          gs' <- execGame gs addBall
          return $ strikes gs' == strikes gs

    it "preserves homeScore" $
      property $
        \(SafeCountState gs) -> ioProperty $ do
          gs' <- execGame gs addBall
          return $ homeScore gs' == homeScore gs

    it "preserves awayScore" $
      property $
        \(SafeCountState gs) -> ioProperty $ do
          gs' <- execGame gs addBall
          return $ awayScore gs' == awayScore gs

  -- -------------------------------------------------------------------------
  -- addStrike invariants
  -- Mirrors: CountInvariants.addStrike_increments, addStrike_preserves_*, etc.
  -- -------------------------------------------------------------------------
  describe "addStrike (strikes ≤ 1, no strikeout triggered)" $ do
    it "increments strikes by 1" $
      property $
        \(SafeCountState gs) -> ioProperty $ do
          gs' <- execGame gs addStrike
          return $ strikes gs' == strikes gs + 1

    it "preserves outs" $
      property $
        \(SafeCountState gs) -> ioProperty $ do
          gs' <- execGame gs addStrike
          return $ outs gs' == outs gs

    it "preserves balls" $
      property $
        \(SafeCountState gs) -> ioProperty $ do
          gs' <- execGame gs addStrike
          return $ balls gs' == balls gs

  -- -------------------------------------------------------------------------
  -- addOut invariants
  -- Mirrors: CountInvariants.addOut_increments, addOut_preserves_*, etc.
  -- -------------------------------------------------------------------------
  describe "addOut (outs ≤ 1, no half-inning change triggered)" $ do
    it "increments outs by 1" $
      property $
        \(SafeCountState gs) -> ioProperty $ do
          gs' <- execGame gs addOut
          return $ outs gs' == outs gs + 1

    it "preserves balls" $
      property $
        \(SafeCountState gs) -> ioProperty $ do
          gs' <- execGame gs addOut
          return $ balls gs' == balls gs

    it "preserves strikes" $
      property $
        \(SafeCountState gs) -> ioProperty $ do
          gs' <- execGame gs addOut
          return $ strikes gs' == strikes gs

  -- -------------------------------------------------------------------------
  -- isGameOver
  -- Mirrors: GameTermination proofs
  -- -------------------------------------------------------------------------
  describe "isGameOver" $ do
    it "home wins: inning >= 9, Bottom, homeScore > awayScore" $
      isGameOver (newGameState {inning = 9, halfInning = Bottom, homeScore = 5, awayScore = 3})
        `shouldBe` True

    it "away wins: inning >= 9, Top, awayScore > homeScore" $
      isGameOver (newGameState {inning = 9, halfInning = Top, awayScore = 5, homeScore = 3})
        `shouldBe` True

    it "not over: tie game inning 9 Bottom" $
      isGameOver (newGameState {inning = 9, halfInning = Bottom, homeScore = 3, awayScore = 3})
        `shouldBe` False

    it "not over: inning 8 even if home winning" $
      isGameOver (newGameState {inning = 8, halfInning = Bottom, homeScore = 5, awayScore = 3})
        `shouldBe` False

    -- isGameOver is only consulted mid-inning; away-wins-at-bottom is handled
    -- by nextHalfInning, not isGameOver.
    it "not over via isGameOver: inning 9 Bottom away winning (handled by nextHalfInning)" $
      isGameOver (newGameState {inning = 9, halfInning = Bottom, awayScore = 5, homeScore = 3})
        `shouldBe` False

  -- -------------------------------------------------------------------------
  -- nextHalfInning regression: away wins at end of bottom of inning >= 9
  --
  -- Bug: the away-winning case fell through to the "advance inning" branch,
  -- incorrectly advancing to inning 10 instead of ending the game.
  -- Fix: added an explicit `return ()` for inning >= 9 && awayWinning.
  -- -------------------------------------------------------------------------
  describe "nextHalfInning regression: away wins at bottom of inning >= 9" $ do
    let awayWinsInNinth =
          newGameState
            { inning = 9,
              halfInning = Bottom,
              awayScore = 5,
              homeScore = 3,
              outs = 2 -- addOut pushes to 3, triggering checkOuts → nextHalfInning
            }

    it "does not advance the inning" $ do
      gs' <- execGame awayWinsInNinth addOut
      inning gs' `shouldBe` 9

    it "does not change halfInning to Top" $ do
      gs' <- execGame awayWinsInNinth addOut
      halfInning gs' `shouldBe` Bottom

    it "also works for extra innings (inning > 9)" $ do
      let extraInnings = awayWinsInNinth {inning = 11, awayScore = 7, homeScore = 6}
      gs' <- execGame extraInnings addOut
      inning gs' `shouldBe` 11

  -- -------------------------------------------------------------------------
  -- Differential Random Testing (Lean reference oracle)
  -- Skipped automatically when the Lean binary is not present.
  -- Build it first: cd lean && lake build DiffTest
  -- -------------------------------------------------------------------------
  leanBinExists <- runIO $ doesFileExist leanBin
  when leanBinExists $
    runDiffTests leanBin
