module Test.BuggyCounter
  ( prop_buggyCounterDetected
  ) where

import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog.Lockstep

-- ---------------------------------------------------------------------------
-- System under test: a counter that wraps at 10
-- ---------------------------------------------------------------------------

newCounter :: IO (IORef Int)
newCounter = newIORef 0

incCounter :: IORef Int -> Int -> IO Int
incCounter ref n = do
  modifyIORef' ref (+ n)
  readIORef ref

getCounter :: IORef Int -> IO Int
getCounter = readIORef

-- ---------------------------------------------------------------------------
-- BUGGY model: doesn't wrap at 10 (but the real system does... wait,
-- actually let's make the model buggy by using a wrong increment)
-- The model always increments by 1, ignoring the actual amount.
-- ---------------------------------------------------------------------------

type Model = Int

-- ---------------------------------------------------------------------------
-- Inputs
-- ---------------------------------------------------------------------------

data IncInput v = IncInput !Int
  deriving stock (Show)

instance FunctorB IncInput where bmap _ (IncInput n) = IncInput n
instance TraversableB IncInput where btraverse _ (IncInput n) = pure (IncInput n)

data GetInput v = GetInput
  deriving stock (Show)

instance FunctorB GetInput where bmap _ GetInput = GetInput
instance TraversableB GetInput where btraverse _ GetInput = pure GetInput

-- ---------------------------------------------------------------------------
-- Commands
-- ---------------------------------------------------------------------------

cmdInc :: IORef Int -> LockstepCmd (PropertyT IO) Model
cmdInc ref = LockstepCmd
  { lsCmdGen = \_ -> Just $ IncInput <$> Gen.int (Range.linear 1 5)
  , lsCmdExec = \(IncInput n) -> evalIO $ incCounter ref n
  , lsCmdModel = \st (IncInput _n) ->
      -- BUG: model always increments by 1
      let model = getModel st
          model' = model + 1
      in (model', model')
  , lsCmdRequire = \_ _ -> True
  , lsCmdObserve = \expected actual -> expected === actual
  , lsCmdInvariants = \_ _ -> pure ()
  , lsCmdTag = \_ _ _ -> []
  }

cmdGet :: IORef Int -> LockstepCmd (PropertyT IO) Model
cmdGet ref = LockstepCmd
  { lsCmdGen = \_ -> Just $ pure GetInput
  , lsCmdExec = \GetInput -> evalIO $ getCounter ref
  , lsCmdModel = \st GetInput ->
      let model = getModel st
      in (model, model)
  , lsCmdRequire = \_ _ -> True
  , lsCmdObserve = \expected actual -> expected === actual
  , lsCmdInvariants = \_ _ -> pure ()
  , lsCmdTag = \_ _ _ -> []
  }

-- ---------------------------------------------------------------------------
-- Property: should FAIL because the model is buggy
--
-- Shrinking expectation (manual inspection only -- not asserted, see
-- below for why):
--
-- A fresh run should shrink to a 1- or 2-action counterexample of the
-- form @[Inc n]@ or @[Inc n, Get]@ where @n >= 2@. The bug is that the
-- model increments by 1 regardless of @n@, so:
--
-- * @Inc 1@ does not surface the bug (model 0+1=1, real 0+1=1).
-- * @Inc 2@ is the smallest @n@ where model (= 1) and real (= 2)
--   diverge.
--
-- A counterexample like @[Inc 5, Inc 4, Get, Inc 3, ...]@ would mean
-- shrinking is not running, which is a real regression. We do not
-- assert that here because:
--
-- 1. Hedgehog's @check@ returns 'Bool', not the shrunk counterexample,
--    so structured access requires @Hedgehog.Internal.Runner@ APIs we
--    do not want to depend on.
-- 2. Different shrinker versions can validly produce @Inc 2@ vs
--    @Inc 3@; both are minimal in spirit.
-- 3. Parsing the rendered failure box couples the test to Hedgehog's
--    output format.
--
-- The assertion we /do/ make below is the meaningful one: the buggy
-- model is detected at all. To eyeball the shrunk shape, run:
--
-- > cabal test --test-show-details=direct
-- ---------------------------------------------------------------------------

prop_buggyCounterDetected :: Property
prop_buggyCounterDetected = withTests 200 $
  lockstepPropertyWith
    (0 :: Model)
    30
    newCounter
    (\ref -> modifyIORef' ref (const 0))
    (\ref -> [cmdInc ref, cmdGet ref])
