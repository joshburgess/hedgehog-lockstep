{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Test.OpProjections
  ( prop_opProjections
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog.Lockstep

-- ---------------------------------------------------------------------------
-- Exercises OpSnd and OpLeft.
--
-- A 'Make' command returns @Either String (Int, Int)@: @Right (n, n*2)@ for
-- non-negative inputs, @Left "negative"@ for negative inputs. Subsequent
-- commands project out:
--
--   * the doubled value via @OpRight >>> OpSnd@   (exercises 'OpSnd')
--   * the error message via @OpLeft@              (exercises 'OpLeft')
--
-- Partial projections: when the underlying 'Either' doesn't match the
-- projection, both 'resolveGVar' (model) and 'concreteGVar' (real) should
-- return 'Nothing', and the model/real must agree.
-- ---------------------------------------------------------------------------

type MakeOutput = Either String (Int, Int)

-- | The "real system" here is just an allocation counter so that outputs
-- differ across repeated calls with the same input; this keeps the model
-- from being trivially stateless.
newRef :: IO (IORef Int)
newRef = newIORef 0

resetRef :: IORef Int -> IO ()
resetRef ref = writeIORef ref 0

data Model = Model { mCalls :: !Int }
  deriving stock (Show)

initialModel :: Model
initialModel = Model 0

-- ---------------------------------------------------------------------------
-- Inputs
-- ---------------------------------------------------------------------------

data MakeInput v = MakeInput !Int
  deriving stock (Show)

instance FunctorB MakeInput where
  bmap _ (MakeInput n) = MakeInput n

instance TraversableB MakeInput where
  btraverse _ (MakeInput n) = pure (MakeInput n)

data ReadDoubledInput v = ReadDoubledInput !(GVar Int v)
  deriving stock (Show)

instance FunctorB ReadDoubledInput where
  bmap f (ReadDoubledInput gv) = ReadDoubledInput (bmap f gv)

instance TraversableB ReadDoubledInput where
  btraverse f (ReadDoubledInput gv) = ReadDoubledInput <$> btraverse f gv

data ReadErrorInput v = ReadErrorInput !(GVar String v)
  deriving stock (Show)

instance FunctorB ReadErrorInput where
  bmap f (ReadErrorInput gv) = ReadErrorInput (bmap f gv)

instance TraversableB ReadErrorInput where
  btraverse f (ReadErrorInput gv) = ReadErrorInput <$> btraverse f gv

-- ---------------------------------------------------------------------------
-- Make: produces @Either String (Int, Int)@
-- ---------------------------------------------------------------------------

make :: IORef Int -> Int -> IO MakeOutput
make ref n = do
  _ <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
  pure $ if n >= 0
    then Right (n, n * 2)
    else Left ("negative: " <> show n)

cmdMake :: IORef Int -> LockstepCmd (PropertyT IO) Model
cmdMake ref = LockstepCmd
  { lsCmdGen = \_ -> Just $ MakeInput <$> Gen.int (Range.linear (-5) 5)

  , lsCmdExec = \(MakeInput n) -> evalIO $ make ref n

  , lsCmdModel = \st (MakeInput n) ->
      let out :: MakeOutput
          out | n >= 0    = Right (n, n * 2)
              | otherwise = Left ("negative: " <> show n)
          m' = (getModel st) { mCalls = mCalls (getModel st) + 1 }
      in (out, m')

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \expected actual -> expected === actual

  , lsCmdInvariants = \_ _ -> pure ()

  , lsCmdTag = \_ _ _ -> []
  }

-- ---------------------------------------------------------------------------
-- ReadDoubled: picks a prior Make result, projects via OpRight >>> OpSnd
-- ---------------------------------------------------------------------------

pickDoubled :: LockstepState Model Symbolic -> Gen (GVar Int Symbolic)
pickDoubled st = do
  let vars = varsOfType @MakeOutput st
  var <- Gen.element vars
  let op :: Op MakeOutput Int
      op = OpRight >>> OpSnd
  pure (mkGVar var op)

cmdReadDoubled :: LockstepCmd (PropertyT IO) Model
cmdReadDoubled = LockstepCmd
  { lsCmdGen = \st ->
      if null (varsOfType @MakeOutput st)
        then Nothing
        else Just $ ReadDoubledInput <$> pickDoubled st

  , lsCmdExec = \(ReadDoubledInput gv) -> pure (concreteGVar gv)

  , lsCmdModel = \st (ReadDoubledInput gv) ->
      (resolveGVar gv (getEntries st), getModel st)

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \expected actual -> expected === actual

  , lsCmdInvariants = \_ _ -> pure ()

  , lsCmdTag = \_ _ _ -> []
  }

-- ---------------------------------------------------------------------------
-- ReadError: picks a prior Make result, projects via OpLeft
-- ---------------------------------------------------------------------------

pickError :: LockstepState Model Symbolic -> Gen (GVar String Symbolic)
pickError st = do
  let vars = varsOfType @MakeOutput st
  var <- Gen.element vars
  let op :: Op MakeOutput String
      op = OpLeft
  pure (mkGVar var op)

cmdReadError :: LockstepCmd (PropertyT IO) Model
cmdReadError = LockstepCmd
  { lsCmdGen = \st ->
      if null (varsOfType @MakeOutput st)
        then Nothing
        else Just $ ReadErrorInput <$> pickError st

  , lsCmdExec = \(ReadErrorInput gv) -> pure (concreteGVar gv)

  , lsCmdModel = \st (ReadErrorInput gv) ->
      (resolveGVar gv (getEntries st), getModel st)

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \expected actual -> expected === actual

  , lsCmdInvariants = \_ _ -> pure ()

  , lsCmdTag = \_ _ _ -> []
  }

-- ---------------------------------------------------------------------------
-- Property
-- ---------------------------------------------------------------------------

prop_opProjections :: Property
prop_opProjections =
  lockstepPropertyWith
    initialModel
    40
    newRef
    resetRef
    (\ref -> [cmdMake ref, cmdReadDoubled, cmdReadError])
