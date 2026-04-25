{-# LANGUAGE GADTs #-}
-- | Structured comparison of model output and real output.
--
-- A t'Observation' value describes /how/ to compare a model-side output
-- (of type @modelOutput@) against the real system's output (of type
-- @output@), without committing to a specific assertion at the call
-- site. The 'runObservation' function turns an t'Observation' into a
-- function suitable for the
-- 'Hedgehog.Lockstep.Command.lsCmdObserve' field of
-- 'Hedgehog.Lockstep.Command.LockstepCmd'.
--
-- This is the 'hedgehog-lockstep' analogue of
-- @quickcheck-lockstep@'s @Observable@\/@ModelValue@ GADT split:
-- t'Observation' factors out the "what does it mean for these two values
-- to agree?" question into a small typed DSL with three primitives
-- ('ObserveEq', 'ObserveProject', 'ObserveCustom') and one combinator
-- ('ObservePair'), all driven by hedgehog's diffable equality
-- ('Hedgehog.===').
--
-- == Why bother
--
-- Plain @lsCmdObserve = (===)@ requires the model and real outputs to
-- have the same type. Real APIs frequently violate that:
--
-- * The real side returns an opaque handle while the model returns an
--   index.
-- * The real side returns a concrete error message while the model
--   returns an abstract error tag.
-- * Both sides return rich records but only a few fields should be
--   compared.
--
-- 'ObserveProject' addresses these by pushing both sides through
-- normalising functions before comparing. 'ObservePair' composes two
-- observations on a tuple. 'ObserveCustom' is the escape hatch when
-- nothing else fits.
module Hedgehog.Lockstep.Observe
  ( Observation (..)
  , runObservation
  ) where

import Hedgehog (Test, (===))

-- | A typed description of how to compare a model output of type
-- @modelOutput@ against a real output of type @output@.
--
-- All constructors carry the 'Eq' and 'Show' instances needed to
-- produce hedgehog's standard diff on mismatch.
data Observation modelOutput output where
  -- | The model and real outputs have the same type and should be
  -- compared by equality. This is the common case for pure model
  -- predictions.
  ObserveEq
    :: (Eq output, Show output)
    => Observation output output

  -- | Project each side through a normalising function and compare the
  -- projections by equality. Use this when the two sides have different
  -- types but a meaningful common observation: comparing only the
  -- relevant fields, normalising error formats, etc.
  --
  -- The two projections @modelOutput -> obs@ and @output -> obs@ must
  -- agree on what counts as "the same" result.
  ObserveProject
    :: (Eq obs, Show obs)
    => (modelOutput -> obs)
    -> (output -> obs)
    -> Observation modelOutput output

  -- | Apply two observations componentwise to a pair. Useful when an
  -- action returns a tuple where each side wants its own normalisation.
  ObservePair
    :: Observation m1 o1
    -> Observation m2 o2
    -> Observation (m1, m2) (o1, o2)

  -- | Drop down to an arbitrary 'Test' action for cases that none of
  -- the structured constructors capture (custom diff messages,
  -- multi-step assertions, classification side-effects, etc.).
  ObserveCustom
    :: (modelOutput -> output -> Test ())
    -> Observation modelOutput output

-- | Interpret an t'Observation' as a check that the model and real
-- outputs agree.
--
-- The result is intentionally shaped to match
-- 'Hedgehog.Lockstep.Command.lsCmdObserve' so users can write
--
-- > lsCmdObserve = runObservation (ObserveProject normaliseModel normaliseReal)
--
-- and skip writing the @===@ call by hand.
runObservation :: Observation modelOutput output -> modelOutput -> output -> Test ()
runObservation obs0 = go obs0
  where
    go :: Observation m o -> m -> o -> Test ()
    go ObserveEq                   m o            = m === o
    go (ObserveProject pm po)      m o            = pm m === po o
    go (ObservePair obsA obsB)     (m1, m2) (o1, o2) = do
      go obsA m1 o1
      go obsB m2 o2
    go (ObserveCustom k)           m o            = k m o
{-# INLINABLE runObservation #-}
