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
  , mapGVar
  , resolveGVar
  , concreteGVar
  , gvarLabel
  ) where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Typeable (Typeable)
import Data.Functor.Classes (Ord1)
import Hedgehog.Internal.State (Var (..), Concrete (..))
import Data.Functor.Barbie (FunctorB (..), TraversableB (..))

import Hedgehog.Lockstep.Op (InterpretOp, interpretOp)
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

-- | Construct a t'GVar' using a typed structural projection.
--
-- @modelX@ is the model-side output type stored in the environment.
-- The op projects from @modelX@ to the desired type @a@. Any op type with
-- an 'InterpretOp' instance and a 'Show' instance works here. The library
-- ships 'Hedgehog.Lockstep.Op.Op' as the default; users can extend the
-- projection vocabulary by defining their own GADT.
mkGVar
  :: (Typeable modelX, Typeable realX, Ord realX, InterpretOp op, Show (op modelX a))
  => Var realX v -> op modelX a -> GVar a v
mkGVar var op =
  GVar var (show op) $ \dyn ->
    fromDynamic dyn >>= interpretOp op
{-# INLINABLE mkGVar #-}

-- | Construct a t'GVar' with an identity projection. Use when the
-- entire model output is the desired value.
mkGVarId
  :: (Typeable a, Typeable realX, Ord realX)
  => Var realX v -> GVar a v
mkGVarId var =
  GVar var "id" fromDynamic
{-# INLINABLE mkGVarId #-}

-- | Compose an additional projection onto an existing t'GVar'.
--
-- Useful when you already hold a t'GVar' to a structured value and want
-- to project further. For example, given a @t'GVar' ('Either' err (h, name)) v@,
-- @'mapGVar' ('Hedgehog.Lockstep.Op.OpRight' 'Hedgehog.Lockstep.Op.>>>' 'Hedgehog.Lockstep.Op.OpFst')@
-- produces a @t'GVar' h v@.
--
-- The new t'GVar' shares the underlying Hedgehog t'Hedgehog.Internal.State.Var'
-- and the existing resolution chain; only the final projection step
-- changes. The human-readable label is extended with the new op.
--
-- This is the 'hedgehog-lockstep' analogue of @quickcheck-lockstep@'s
-- @mapGVar@.
mapGVar
  :: forall op a b v. (InterpretOp op, Show (op a b))
  => op a b -> GVar a v -> GVar b v
mapGVar op (GVar var label resolve) =
  GVar var (label <> "." <> show op) (\dyn -> resolve dyn >>= interpretOp op)
{-# INLINABLE mapGVar #-}

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
