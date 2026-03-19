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
  }

-- ---------------------------------------------------------------------------
-- Property: should FAIL because the model is buggy
-- ---------------------------------------------------------------------------

prop_buggyCounterDetected :: Property
prop_buggyCounterDetected = withTests 200 $
  lockstepPropertyWith
    (0 :: Model)
    30
    newCounter
    (\ref -> modifyIORef' ref (const 0))
    (\ref -> [cmdInc ref, cmdGet ref])
