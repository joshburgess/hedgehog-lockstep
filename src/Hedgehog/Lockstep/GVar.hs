{-# LANGUAGE RankNTypes #-}
-- | Generalized variables: typed projections from prior action outputs.
--
-- A t'GVar' wraps a Hedgehog t'Hedgehog.Internal.State.Var' together with
-- an 'Hedgehog.Lockstep.Op.Op' that projects a component out of the
-- action's result. Later commands use t'GVar's to refer to those
-- components in both the model and the real system.
module Hedgehog.Lockstep.GVar
  ( GVar (..)
  , mkGVar
  , mkGVarId
  , resolveGVar
  , concreteGVar
  , gvarLabel
  ) where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Typeable (Typeable)
import Data.Functor.Classes (Ord1)
import Hedgehog.Internal.State (Var (..), Concrete (..))
import Data.Functor.Barbie (FunctorB (..), TraversableB (..))

import Hedgehog.Lockstep.Op (Op, applyOp, showOp)
import Hedgehog.Lockstep.State (ModelEnv, lookupModelEntry)

-- | A generalized variable that projects a value of type @a@ from a
-- previous action's output via the model environment.
--
-- During generation (t'Hedgehog.Internal.State.Symbolic' phase), the
-- t'GVar' tracks which variable it references. During execution
-- (t'Hedgehog.Internal.State.Concrete' phase), the underlying t'Hedgehog.Internal.State.Var'
-- resolves to the real value, but model-side resolution always goes
-- through the model environment via t'Hedgehog.Internal.State.Var'
-- identity matching.
data GVar a v where
  GVar
    :: (Typeable x, Ord x)
    => !(Var x v)                  -- ^ Underlying Hedgehog variable
    -> !String                     -- ^ Human-readable projection label
    -> !(Dynamic -> Maybe a)       -- ^ Resolution function from model env entry
    -> GVar a v

instance FunctorB (GVar a) where
  bmap f (GVar (Var v) label res) = GVar (Var (f v)) label res

instance TraversableB (GVar a) where
  btraverse f (GVar (Var v) label res) =
    (\v' -> GVar (Var v') label res) <$> f v

instance Show (GVar a v) where
  show (GVar _ label _) = "GVar." <> label

-- | Construct a t'GVar' using a typed 'Op' projection.
--
-- @modelX@ is the model-side output type stored in the environment.
-- The 'Op' projects from @modelX@ to the desired type @a@.
mkGVar
  :: (Typeable modelX, Typeable realX, Ord realX)
  => Var realX v -> Op modelX a -> GVar a v
mkGVar var op =
  GVar var (showOp op) $ \dyn ->
    fromDynamic dyn >>= applyOp op
{-# INLINABLE mkGVar #-}

-- | Construct a t'GVar' with an identity projection. Use when the
-- entire model output is the desired value.
mkGVarId
  :: (Typeable a, Typeable realX, Ord realX)
  => Var realX v -> GVar a v
mkGVarId var =
  GVar var "id" fromDynamic
{-# INLINABLE mkGVarId #-}

-- | Resolve a t'GVar' against a model environment.
-- Uses 'Ord1' for phase-polymorphic t'Hedgehog.Internal.State.Var'
-- comparison.
resolveGVar :: Ord1 v => GVar a v -> ModelEnv v -> Maybe a
resolveGVar (GVar var _ resolve) entries =
  lookupModelEntry var entries >>= resolve
{-# INLINABLE resolveGVar #-}

-- | Extract the projected value from a t'GVar' in the t'Hedgehog.Internal.State.Concrete' phase.
--
-- This applies the resolution function to the real output value. Works when
-- the real and model types share the same structure at the projected position
-- (e.g., both are @Either String (Int, String)@).
concreteGVar :: GVar a Concrete -> Maybe a
concreteGVar (GVar (Var (Concrete x)) _ resolve) = resolve (toDyn x)
{-# INLINABLE concreteGVar #-}

-- | Get the human-readable label of a t'GVar'.
gvarLabel :: GVar a v -> String
gvarLabel (GVar _ label _) = label
