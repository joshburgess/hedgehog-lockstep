module Hedgehog.Lockstep
  ( -- * Commands
    LockstepCmd (..)
  , toLockstepCommand
  , lockstepCommands

    -- * State
  , LockstepState (..)
  , ModelEntry (..)
  , initialLockstepState
  , getModel
  , getEntries
  , varsOfType

    -- * Generalized variables
  , GVar (..)
  , mkGVar
  , mkGVarId
  , resolveGVar
  , concreteGVar

    -- * Structural projections
  , Op (..)
  , applyOp
  , (>>>)

    -- * Running tests
  , lockstepProperty
  , lockstepPropertyWith
  , lockstepParallel

    -- * Re-exports from Hedgehog
  , Var (..)
  , Symbolic
  , Concrete
  , Gen
  , Test
  , Property
  , PropertyT
  , (===)

    -- * Re-exports from barbies
  , FunctorB (..)
  , TraversableB (..)
  ) where

import Data.Functor.Barbie (FunctorB (..), TraversableB (..))
import Hedgehog (Gen, Test, Property, PropertyT, (===))
import Hedgehog.Internal.State (Var (..), Symbolic, Concrete)

import Hedgehog.Lockstep.Op
import Hedgehog.Lockstep.GVar
import Hedgehog.Lockstep.State
import Hedgehog.Lockstep.Command
import Hedgehog.Lockstep.Property
