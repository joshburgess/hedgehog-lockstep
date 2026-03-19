{-# LANGUAGE RankNTypes #-}
module Hedgehog.Lockstep.State
  ( LockstepState (..)
  , ModelEntry (..)
  , SomeVar (..)
  , initialLockstepState
  , getModel
  , getEntries
  , getNextVarId
  , varsOfType
  , insertModelResult
  , lookupModelEntry
  ) where

import Data.Dynamic (Dynamic, toDyn)
import Data.Typeable (Typeable, eqT)
import Type.Reflection ((:~:) (..))
import Data.Functor.Classes (Ord1, liftCompare)
import Hedgehog.Internal.State (Var (..), Symbolic)

-- | An entry in the model environment, storing a Var and its model result.
data ModelEntry v where
  ModelEntry
    :: (Typeable a, Ord a)
    => !(Var a v)
    -> !Dynamic      -- ^ Model result for this variable
    -> ModelEntry v

-- | An existentially-wrapped 'Var' with its type witness.
data SomeVar v where
  SomeVar :: Typeable a => !(Var a v) -> SomeVar v

-- | Lockstep state wrapping a user-defined model.
--
-- Tracks the pure model state alongside a model environment that maps
-- variables to model-side output values. This enables 'GVar' resolution
-- during both generation and execution phases, including after shrinking.
data LockstepState model v = LockstepState
  { lsModel     :: !model
  , lsNextVarId :: {-# UNPACK #-} !Int
  , lsEntries   :: ![ModelEntry v]
  , lsVars      :: ![SomeVar v]
  }

instance Show model => Show (LockstepState model v) where
  showsPrec p st = showParen (p > 10) $
    showString "LockstepState {model = " .
    showsPrec 0 (lsModel st) .
    showString ", vars = " .
    showsPrec 0 (length (lsVars st)) .
    showString "}"

-- | Create the initial lockstep state from a model value.
initialLockstepState :: model -> LockstepState model v
initialLockstepState m = LockstepState
  { lsModel     = m
  , lsNextVarId = 0
  , lsEntries   = []
  , lsVars      = []
  }

-- | Extract the user's model state.
getModel :: LockstepState model v -> model
getModel = lsModel

-- | Extract the model entries.
getEntries :: LockstepState model v -> [ModelEntry v]
getEntries = lsEntries

-- | Get the next variable ID (used internally for Ensure lookup).
getNextVarId :: LockstepState model v -> Int
getNextVarId = lsNextVarId

-- | Enumerate all variables of a given type.
-- Useful in generators to pick a variable for a 'GVar'.
varsOfType
  :: forall a model. Typeable a
  => LockstepState model Symbolic -> [Var a Symbolic]
varsOfType st =
  [ var
  | SomeVar (var :: Var b Symbolic) <- lsVars st
  , Just Refl <- [eqT @a @b]
  ]

-- | Look up a model result by matching the 'Var' identity.
-- Uses 'Ord1' for phase-polymorphic comparison:
-- for 'Symbolic', compares by hedgehog Name (stable across shrinking);
-- for 'Concrete', compares by value.
lookupModelEntry
  :: forall x v. (Typeable x, Ord x, Ord1 v)
  => Var x v -> [ModelEntry v] -> Maybe Dynamic
lookupModelEntry var entries = go entries
  where
    go [] = Nothing
    go (ModelEntry (entryVar :: Var b v) dyn : rest) =
      case eqT @x @b of
        Just Refl
          | varEq var entryVar -> Just dyn
        _ -> go rest

    varEq :: Var x v -> Var x v -> Bool
    varEq (Var v1) (Var v2) = liftCompare compare v1 v2 == EQ

-- | Insert a model result into the state and register the variable.
-- Used internally by 'toLockstepCommand'.
insertModelResult
  :: (Typeable modelOutput, Typeable output, Ord output)
  => Var output v -> modelOutput -> LockstepState model v -> LockstepState model v
insertModelResult var modelOut st =
  let varId = lsNextVarId st
  in st
    { lsNextVarId = varId + 1
    , lsEntries   = ModelEntry var (toDyn modelOut) : lsEntries st
    , lsVars      = SomeVar var : lsVars st
    }
