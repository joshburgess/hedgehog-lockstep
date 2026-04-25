-- | Lockstep-style stateful property testing for Hedgehog.
--
-- This is the top-level facade. Most users will only need to import this
-- module. It re-exports the public API of 'Hedgehog.Lockstep.Command',
-- 'Hedgehog.Lockstep.Property', 'Hedgehog.Lockstep.State',
-- 'Hedgehog.Lockstep.GVar', and 'Hedgehog.Lockstep.Op', plus the small
-- set of Hedgehog and barbies identifiers needed to write commands.
--
-- A short tour:
--
-- * t'LockstepCmd' is the unit of testing: an API operation with its
--   model interpretation, observation, and invariants.
-- * 'lockstepProperty' / 'lockstepPropertyWith' run sequential tests.
-- * 'lockstepParallel' / 'lockstepParallelWith' run linearizability tests.
-- * t'GVar' with t'Op' projects values out of compound action outputs so
--   later commands can refer to them.
--
-- See the README and "Hedgehog.Lockstep.Command" for fuller explanations.
module Hedgehog.Lockstep
  ( -- * Commands
    LockstepCmd (..)
  , toLockstepCommand
  , hoistLockstepCmd
  , lockstepCommands

    -- * State
  , LockstepState
  , ModelEnv
  , initialLockstepState
  , getModel
  , getEntries
  , varsOfType

    -- * Generalized variables
  , GVar (..)
  , mkGVar
  , mkGVarId
  , mapGVar
  , resolveGVar
  , concreteGVar
  , gvarLabel

    -- * Structural projections
  , Op (..)
  , InterpretOp (..)
  , applyOp
  , (>>>)

    -- * Structured observations
  , Observation (..)
  , runObservation

    -- * Running tests
  , lockstepProperty
  , lockstepPropertyWith
  , lockstepParallel
  , lockstepParallelWith

    -- * Running tests in arbitrary monads
  , lockstepPropertyM
  , lockstepPropertyWithM
  , lockstepParallelM
  , lockstepParallelWithM

    -- * Coverage exploration
  , lockstepLabelledExamples
  , LabelledExamples
  , ModelStep (..)

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
import Hedgehog.Lockstep.Observe
import Hedgehog.Lockstep.State
import Hedgehog.Lockstep.Command
import Hedgehog.Lockstep.Examples
import Hedgehog.Lockstep.Property
