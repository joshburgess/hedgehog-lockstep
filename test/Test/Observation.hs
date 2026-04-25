{-# LANGUAGE RankNTypes #-}
module Test.Observation
  ( prop_observation
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog.Lockstep

-- ---------------------------------------------------------------------------
-- Exercises 'Observation' beyond plain equality.
--
-- The system under test returns @(Int, String)@ where the @String@ is a
-- formatted, human-readable version of the @Int@ ("count=42"). The
-- model ignores formatting concerns and returns @(Int, Int)@. The
-- observation projects both sides to a common @Int@ shape via
-- 'ObservePair' + 'ObserveProject' + 'ObserveEq'.
-- ---------------------------------------------------------------------------

type Counter = IORef Int

type Model = Int

newCounter :: IO Counter
newCounter = newIORef 0

resetCounter :: Counter -> IO ()
resetCounter ref = writeIORef ref 0

bump :: Counter -> Int -> IO (Int, String)
bump ref n = do
  modifyIORef' ref (+ n)
  v <- readIORef ref
  pure (v, "count=" <> show v)

-- Inputs

data BumpInput v = BumpInput !Int
  deriving stock (Show)

instance FunctorB BumpInput where bmap _ (BumpInput n) = BumpInput n
instance TraversableB BumpInput where btraverse _ (BumpInput n) = pure (BumpInput n)

-- The model returns @(Int, Int)@ (the second Int is the same as the
-- first, deliberately matching the textual count). The real output is
-- @(Int, String)@. ObservePair lets us compare the @Int@ slot directly
-- and project the formatted @String@ slot back to @Int@ before comparing.
cmdBump :: Counter -> LockstepCmd (PropertyT IO) Model
cmdBump ref = LockstepCmd
  { lsCmdGen = \_ -> Just $ BumpInput <$> Gen.int (Range.linear 1 5)

  , lsCmdExec = \(BumpInput n) -> evalIO (bump ref n)

  , lsCmdModel = \st (BumpInput n) ->
      let v = getModel st + n
      in ((v, v), v)

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve =
      runObservation $
        ObservePair
          ObserveEq
          (ObserveProject id parseCount)

  , lsCmdInvariants = \_ _ -> pure ()

  , lsCmdTag = \_ _ _ -> []
  }
  where
    -- Strip "count=" prefix from the formatted string.
    parseCount :: String -> Int
    parseCount s =
      case drop (length ("count=" :: String)) s of
        digits -> read digits

prop_observation :: Property
prop_observation =
  lockstepPropertyWith
    0
    20
    newCounter
    resetCounter
    (\ref -> [cmdBump ref])
