{-# LANGUAGE TypeApplications #-}
-- | Demonstrates that users can extend the projection vocabulary beyond
-- the built-in 'Op' GADT.
--
-- Defines @MyOp@, a user GADT that wraps the default 'Op' and adds a
-- @MyOpHead :: MyOp [a] a@ constructor for projecting the head of a list.
-- The 'InterpretOp' instance plus a 'Show' instance is all that's needed
-- to plug it into 'mkGVar' and 'mapGVar'.
--
-- The integration property runs a small lockstep test where:
--
--   * @ListOf@ produces a non-empty @[Int]@.
--   * @ReadHead@ picks a prior @ListOf@ result and reads its head via
--     @MyOpHead@.
--
-- Real and model agree on every step, demonstrating end-to-end use of a
-- custom op including substitution during shrinking.
module Test.CustomOp
  ( prop_customOpUnit
  , prop_customOpIntegration
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog.Lockstep

-- ---------------------------------------------------------------------------
-- A user-defined op that extends 'Op' with a list-head projection.
-- ---------------------------------------------------------------------------

data MyOp a b where
  MyOfOp   :: !(Op a b) -> MyOp a b
  MyOpHead :: MyOp [a] a

instance InterpretOp MyOp where
  interpretOp (MyOfOp op) x        = interpretOp op x
  interpretOp MyOpHead    []       = Nothing
  interpretOp MyOpHead    (x : _)  = Just x

instance Show (MyOp a b) where
  show (MyOfOp op) = show op
  show MyOpHead    = "head"

-- ---------------------------------------------------------------------------
-- Unit test: the new op flows through the GVar machinery.
-- ---------------------------------------------------------------------------

prop_customOpUnit :: Property
prop_customOpUnit = withTests 1 $ property $ do
  -- applyOp directly:
  applyOp (MyOpHead :: MyOp [Int] Int) [7, 9, 11]   === Just 7
  applyOp (MyOpHead :: MyOp [Int] Int) ([] :: [Int]) === Nothing
  -- via a Concrete-phase GVar, including the label:
  let v :: Var [Int] Concrete
      v  = Var (Concrete [7, 9, 11])
      gv :: GVar Int Concrete
      gv = mkGVar v (MyOpHead :: MyOp [Int] Int)
  gvarLabel gv     === "head"
  concreteGVar gv  === Just 7
  -- mapGVar with the wrapped default ops still works on the same GVar
  -- type, mixing wrapped and custom projections in one chain.
  let vPair :: Var (Int, Int) Concrete
      vPair = Var (Concrete (4, 5))
      gvPair :: GVar Int Concrete
      gvPair = mkGVar vPair (MyOfOp OpFst :: MyOp (Int, Int) Int)
  gvarLabel gvPair === "fst"
  concreteGVar gvPair === Just 4

-- ---------------------------------------------------------------------------
-- Integration test: a lockstep property using MyOpHead in a real command.
-- ---------------------------------------------------------------------------

type Model = ()

data ListOfInput v = ListOfInput ![Int]
  deriving stock (Show)

instance FunctorB ListOfInput where
  bmap _ (ListOfInput xs) = ListOfInput xs
instance TraversableB ListOfInput where
  btraverse _ (ListOfInput xs) = pure (ListOfInput xs)

data ReadHeadInput v = ReadHeadInput !(GVar Int v)
  deriving stock (Show)

instance FunctorB ReadHeadInput where
  bmap f (ReadHeadInput gv) = ReadHeadInput (bmap f gv)
instance TraversableB ReadHeadInput where
  btraverse f (ReadHeadInput gv) = ReadHeadInput <$> btraverse f gv

-- The real system stores a list in an IORef; commands return either a
-- list ('ListOf') or a single Int ('ReadHead').
cmdListOf :: IORef [Int] -> LockstepCmd (PropertyT IO) Model
cmdListOf ref = LockstepCmd
  { lsCmdGen = \_ -> Just $ do
      -- Generate at least one element so MyOpHead never legitimately fails.
      n  <- Gen.int (Range.linear 1 5)
      xs <- Gen.list (Range.singleton n) (Gen.int (Range.linear 0 100))
      pure (ListOfInput xs)
  , lsCmdExec = \(ListOfInput xs) -> evalIO (writeIORef ref xs) >> pure xs
  , lsCmdModel = \_ (ListOfInput xs) -> (xs, ())
  , lsCmdRequire    = \_ _ -> True
  , lsCmdObserve    = runObservation ObserveEq
  , lsCmdInvariants = \_ _ -> pure ()
  , lsCmdTag        = \_ _ _ -> []
  }

cmdReadHead :: IORef [Int] -> LockstepCmd (PropertyT IO) Model
cmdReadHead ref = LockstepCmd
  { lsCmdGen = \st ->
      case varsOfType @[Int] st of
        []   -> Nothing
        vars -> Just $ do
          var <- Gen.element vars
          let gv :: GVar Int Symbolic
              gv = mkGVar var (MyOpHead :: MyOp [Int] Int)
          pure (ReadHeadInput gv)
  , lsCmdExec = \(ReadHeadInput gv) -> do
      stored <- evalIO (readIORef ref)
      case (concreteGVar gv, stored) of
        (Just n, _ : _) -> pure (Just n)
        (_,      _    ) -> pure Nothing
  , lsCmdModel = \st (ReadHeadInput gv) ->
      (resolveGVar gv (getEntries st), ())
  , lsCmdRequire    = \_ _ -> True
  , lsCmdObserve    = runObservation ObserveEq
  , lsCmdInvariants = \_ _ -> pure ()
  , lsCmdTag        = \_ _ _ -> []
  }

prop_customOpIntegration :: Property
prop_customOpIntegration =
  lockstepPropertyWith
    ()
    20
    (newIORef [])
    (\ref -> writeIORef ref [])
    (\ref -> [cmdListOf ref, cmdReadHead ref])
