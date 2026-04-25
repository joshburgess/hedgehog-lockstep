{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The lockstep state: the user's pure model alongside a model environment
-- that maps Hedgehog t'Hedgehog.Internal.State.Var's to their model-side
-- output values.
--
-- The model environment is what makes t'Hedgehog.Lockstep.GVar.GVar'
-- resolution work across the symbolic and concrete phases.
module Hedgehog.Lockstep.State
  ( LockstepState (..)
  , ModelEnv
  , SomeVar (..)
  , initialLockstepState
  , getModel
  , getEntries
  , getNextVarId
  , getLastEntry
  , varsOfType
  , insertModelResult
  , lookupModelEntry
  ) where

import Data.Dynamic (Dynamic, toDyn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, Typeable, eqT, typeRep)
import Type.Reflection ((:~:) (..))
import Data.Functor.Classes (Ord1, liftCompare)
import Hedgehog.Internal.State (Var (..), Symbolic)

-- | Existential variable key with phase-polymorphic ordering.
--
-- Internal: not exposed in the public API. The 'Ord' instance compares
-- by type first, then by phase-specific identity ('Hedgehog.Internal.State.Name'
-- for t'Hedgehog.Internal.State.Symbolic', value for
-- t'Hedgehog.Internal.State.Concrete').
data VarKey v where
  VarKey :: (Typeable a, Ord a) => !(Var a v) -> VarKey v

instance Ord1 v => Eq (VarKey v) where
  VarKey (Var v1 :: Var x v) == VarKey (Var v2 :: Var y v) =
    case eqT @x @y of
      Just Refl -> liftCompare compare v1 v2 == EQ
      Nothing   -> False

instance Ord1 v => Ord (VarKey v) where
  compare (VarKey (Var v1 :: Var x v)) (VarKey (Var v2 :: Var y v)) =
    case eqT @x @y of
      Just Refl -> liftCompare compare v1 v2
      Nothing   -> compare (typeRep (Proxy @x)) (typeRep (Proxy @y))

-- | The model environment: associates t'Hedgehog.Internal.State.Var's
-- with their model-side output values. Stored as a 'Data.Map.Strict.Map'
-- so lookups are @O(log n)@.
--
-- Opaque: construct via 'insertModelResult' and look up via
-- 'lookupModelEntry' or 'Hedgehog.Lockstep.GVar.resolveGVar'.
newtype ModelEnv v = ModelEnv (Map (VarKey v) Dynamic)

-- | An existentially-wrapped t'Hedgehog.Internal.State.Var' with its
-- type witness.
data SomeVar v where
  SomeVar :: Typeable a => !(Var a v) -> SomeVar v

-- | Lockstep state wrapping a user-defined model.
--
-- Tracks the pure model state alongside a model environment that maps
-- variables to model-side output values. This enables
-- t'Hedgehog.Lockstep.GVar.GVar' resolution during both generation and
-- execution phases, including after shrinking.
data LockstepState model v = LockstepState
  { lsModel     :: !model
  , lsNextVarId :: {-# UNPACK #-} !Int
  , lsEntries   :: !(ModelEnv v)
  , lsVars      :: !(Map TypeRep [SomeVar v])
  -- ^ Variables grouped by type, so 'varsOfType' is a single 'Map'
  -- lookup rather than a linear filter over all variables.
  , lsLastEntry :: !(Maybe Dynamic)
  -- ^ The most recently inserted model result, used by the @Ensure@
  -- callback to compare against the real output.
  }

instance Show model => Show (LockstepState model v) where
  showsPrec p st = showParen (p > 10) $
    showString "LockstepState {model = " .
    showsPrec 0 (lsModel st) .
    showString ", vars = " .
    showsPrec 0 (sum (map length (Map.elems (lsVars st)))) .
    showString "}"

-- | Create the initial lockstep state from a model value.
initialLockstepState :: model -> LockstepState model v
initialLockstepState m = LockstepState
  { lsModel     = m
  , lsNextVarId = 0
  , lsEntries   = ModelEnv Map.empty
  , lsVars      = Map.empty
  , lsLastEntry = Nothing
  }

-- | Extract the user's model state.
getModel :: LockstepState model v -> model
getModel = lsModel

-- | Extract the model environment.
getEntries :: LockstepState model v -> ModelEnv v
getEntries = lsEntries

-- | Get the next variable ID (used internally for Ensure lookup).
getNextVarId :: LockstepState model v -> Int
getNextVarId = lsNextVarId

-- | Get the most recently inserted model result, if any. Used by the
-- @Ensure@ callback to compare against the real output.
getLastEntry :: LockstepState model v -> Maybe Dynamic
getLastEntry = lsLastEntry

-- | Enumerate all variables of a given type. @O(log n)@ in the number
-- of distinct types ever inserted, plus @O(k)@ in the number of
-- matching variables.
--
-- Useful in generators to pick a variable for a
-- t'Hedgehog.Lockstep.GVar.GVar'.
varsOfType
  :: forall a model. Typeable a
  => LockstepState model Symbolic -> [Var a Symbolic]
varsOfType st =
  case Map.lookup (typeRep (Proxy @a)) (lsVars st) of
    Nothing     -> []
    Just bucket ->
      [ var
      | SomeVar (var :: Var b Symbolic) <- bucket
      , Just Refl <- [eqT @a @b]
      ]
{-# INLINABLE varsOfType #-}

-- | Look up a model result by t'Hedgehog.Internal.State.Var' identity.
-- @O(log n)@ in the size of the model environment.
lookupModelEntry
  :: forall x v. (Typeable x, Ord x, Ord1 v)
  => Var x v -> ModelEnv v -> Maybe Dynamic
lookupModelEntry var (ModelEnv m) = Map.lookup (VarKey var) m
{-# INLINABLE lookupModelEntry #-}

-- | Insert a model result into the state and register the variable.
-- Used internally by 'Hedgehog.Lockstep.Command.toLockstepCommand'.
insertModelResult
  :: forall modelOutput output model v.
     (Typeable modelOutput, Typeable output, Ord output, Ord1 v)
  => Var output v -> modelOutput -> LockstepState model v -> LockstepState model v
insertModelResult var modelOut st =
  let varId = lsNextVarId st
      dyn   = toDyn modelOut
      tyRep = typeRep (Proxy @output)
      ModelEnv entries = lsEntries st
  in st
    { lsNextVarId = varId + 1
    , lsEntries   = ModelEnv (Map.insert (VarKey var) dyn entries)
    , lsVars      = Map.insertWith (++) tyRep [SomeVar var] (lsVars st)
    , lsLastEntry = Just dyn
    }
{-# INLINABLE insertModelResult #-}
