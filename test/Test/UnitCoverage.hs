{-# LANGUAGE TypeApplications #-}
module Test.UnitCoverage
  ( prop_applyOp
  , prop_gvarLabel
  , prop_mapGVar
  , prop_mkGVarId
  ) where

import Data.IORef (IORef, newIORef, writeIORef)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog.Lockstep

-- ---------------------------------------------------------------------------
-- applyOp: direct unit tests for each Op constructor
-- ---------------------------------------------------------------------------

prop_applyOp :: Property
prop_applyOp = withTests 1 $ property $ do
  applyOp (OpId :: Op Int Int) 7   === Just 7
  applyOp OpFst (1 :: Int, 'a')    === Just 1
  applyOp OpSnd (1 :: Int, 'a')    === Just 'a'
  applyOp OpLeft  (Left  'x' :: Either Char Int) === Just 'x'
  applyOp OpLeft  (Right 9   :: Either Char Int) === Nothing
  applyOp OpRight (Left  'x' :: Either Char Int) === Nothing
  applyOp OpRight (Right 9   :: Either Char Int) === Just 9
  applyOp (OpRight >>> OpFst)
          (Right (1 :: Int, 'a') :: Either String (Int, Char))
    === Just 1

-- ---------------------------------------------------------------------------
-- gvarLabel: render projection chains
-- ---------------------------------------------------------------------------

prop_gvarLabel :: Property
prop_gvarLabel = withTests 1 $ property $ do
  -- Use a Concrete-phase Var so we can construct it directly. The label
  -- only depends on the projection, not on the underlying variable.
  let v :: Var (Either String (Int, Int)) Concrete
      v = Var (Concrete (Right (1, 2)))
  gvarLabel (mkGVarId v :: GVar (Either String (Int, Int)) Concrete) === "id"
  gvarLabel (mkGVar v (OpRight :: Op (Either String (Int, Int)) (Int, Int)))
    === "right"
  gvarLabel (mkGVar v ((OpRight >>> OpFst) :: Op (Either String (Int, Int)) Int))
    === "right.fst"

-- ---------------------------------------------------------------------------
-- mapGVar: composing an additional projection onto an existing GVar
--
-- Build a GVar that projects the @Right@ branch of an @Either@, then use
-- 'mapGVar' to extend it with @OpFst@ to land on the first tuple element.
-- The resulting GVar resolves to that element via 'concreteGVar' and
-- carries the composed projection in its label.
-- ---------------------------------------------------------------------------

prop_mapGVar :: Property
prop_mapGVar = withTests 1 $ property $ do
  let v :: Var (Either String (Int, Int)) Concrete
      v = Var (Concrete (Right (7, 9)))
      gvRight :: GVar (Int, Int) Concrete
      gvRight = mkGVar v (OpRight :: Op (Either String (Int, Int)) (Int, Int))
      gvFst :: GVar Int Concrete
      gvFst = mapGVar OpFst gvRight
  gvarLabel gvFst    === "right.fst"
  concreteGVar gvFst === Just 7
  -- Mapping with OpId is the identity (modulo the label suffix).
  concreteGVar (mapGVar OpId gvRight) === Just (7, 9)

-- ---------------------------------------------------------------------------
-- mkGVarId: an integration test using identity projection
--
-- The action returns a single Int. A later command takes a GVar Int that
-- references the prior result via mkGVarId (no projection). The Echo
-- command simply returns the GVar's concrete value, demonstrating that
-- mkGVarId resolves correctly across both phases.
-- ---------------------------------------------------------------------------

type Model = ()

data PutInput v = PutInput !Int
  deriving stock (Show)

instance FunctorB PutInput where bmap _ (PutInput n) = PutInput n
instance TraversableB PutInput where btraverse _ (PutInput n) = pure (PutInput n)

data EchoInput v = EchoInput !(GVar Int v)
  deriving stock (Show)

instance FunctorB EchoInput where
  bmap f (EchoInput gv) = EchoInput (bmap f gv)
instance TraversableB EchoInput where
  btraverse f (EchoInput gv) = EchoInput <$> btraverse f gv

cmdPut :: IORef Int -> LockstepCmd (PropertyT IO) Model
cmdPut ref = LockstepCmd
  { lsCmdGen        = \_ -> Just $ PutInput <$> Gen.int (Range.linear 0 100)
  , lsCmdExec       = \(PutInput n) -> evalIO (writeIORef ref n) >> pure n
  , lsCmdModel      = \_ (PutInput n) -> (n, ())
  , lsCmdRequire    = \_ _ -> True
  , lsCmdObserve    = \expected actual -> expected === actual
  , lsCmdInvariants = \_ _ -> pure ()
  , lsCmdTag        = \_ _ _ -> []
  }

cmdEcho :: LockstepCmd (PropertyT IO) Model
cmdEcho = LockstepCmd
  { lsCmdGen = \st ->
      case varsOfType @Int st of
        []   -> Nothing
        vars -> Just $ do
          var <- Gen.element vars
          pure $ EchoInput (mkGVarId var)

  , lsCmdExec = \(EchoInput gv) ->
      pure (concreteGVar gv)

  , lsCmdModel = \st (EchoInput gv) ->
      (resolveGVar gv (getEntries st), ())

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \expected actual -> expected === actual

  , lsCmdInvariants = \_ _ -> pure ()
  , lsCmdTag        = \_ _ _ -> []
  }

prop_mkGVarId :: Property
prop_mkGVarId =
  lockstepPropertyWith
    ()
    20
    (newIORef 0)
    (\ref -> writeIORef ref 0)
    (\ref -> [cmdPut ref, cmdEcho])
