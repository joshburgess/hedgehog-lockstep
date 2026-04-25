{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The t'LockstepCmd' record bundling a single API operation with its
-- model interpretation, observation, and post-state invariants.
--
-- Construct one t'LockstepCmd' per operation in your API. Convert the
-- list to Hedgehog t'Hedgehog.Internal.State.Command's via
-- 'toLockstepCommand' (or use
-- 'Hedgehog.Lockstep.Property.lockstepCommands' to do it in bulk).
module Hedgehog.Lockstep.Command
  ( LockstepCmd (..)
  , toLockstepCommand
  , hoistLockstepCmd
  ) where

import Data.Dynamic (dynTypeRep, fromDynamic)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Typeable (TypeRep, Typeable, typeRep)
import Data.Functor.Barbie (TraversableB)
import Data.Functor.Classes (Ord1)
import Hedgehog (Gen, Test, footnote, failure, label)
import Hedgehog.Internal.State
  ( Command (..)
  , Callback (..)
  , Symbolic
  , Concrete
  )

import Hedgehog.Lockstep.State

-- | A lockstep command packages a single API operation with its model
-- interpretation and observation function.
--
-- The key idea: in the 'Update' callback, the library runs 'lsCmdModel' to
-- advance the model state and stores the model result. In 'Ensure', it
-- retrieves the stored model result and compares it with the real output
-- via 'lsCmdObserve'.
--
-- == Type constraints
--
-- The existential constraints on @output@ and @modelOutput@ are:
--
-- * @'Typeable' output@, @'Ord' output@: required by the model environment,
--   which keys entries by 'Hedgehog.Internal.State.Var' identity. The 'Ord'
--   instance is used by 'Data.Functor.Classes.Ord1' for phase-polymorphic
--   'Hedgehog.Internal.State.Var' comparison.
--
-- * @'Typeable' modelOutput@, @'Eq' modelOutput@, @'Show' modelOutput@: the
--   model-side result is stored as 'Data.Dynamic.Dynamic' and may be compared
--   or displayed in error reports.
--
-- The @'Ord' output@ constraint is a real limitation: truly opaque types like
-- @'System.IO.Handle'@ that lack an 'Ord' instance need a newtype wrapper that
-- defines ordering (for example, by an allocation counter).
data LockstepCmd m model = forall input output modelOutput.
  ( TraversableB input
  , Show (input Symbolic)
  , Show output
  , Typeable output
  , Ord output
  , Typeable modelOutput
  , Eq modelOutput
  , Show modelOutput
  ) => LockstepCmd
  { -- | Generate an input given the current lockstep state.
    -- Return 'Nothing' if this command is not applicable in the current state.
    lsCmdGen :: LockstepState model Symbolic -> Maybe (Gen (input Symbolic))

    -- | Execute the command against the real system.
  , lsCmdExec :: input Concrete -> m output

    -- | Run the model step. Given the current lockstep state and input,
    -- return the model's predicted output and the updated model.
    --
    -- This runs in @forall v. Ord1 v@ context. Use
    -- 'Hedgehog.Lockstep.GVar.resolveGVar' with
    -- 'Hedgehog.Lockstep.State.getEntries' to resolve
    -- t'Hedgehog.Lockstep.GVar.GVar's in the input to their model values.
  , lsCmdModel :: forall v. Ord1 v => LockstepState model v -> input v -> (modelOutput, model)

    -- | Additional preconditions beyond variable-definedness.
  , lsCmdRequire :: model -> input Symbolic -> Bool

    -- | Compare model output with real output.
    -- Use Hedgehog assertions ('Hedgehog.===' etc.) to report mismatches.
    --
    -- For structured comparison patterns (equality, projection,
    -- pairwise), 'Hedgehog.Lockstep.Observe.runObservation' produces
    -- a function of exactly this type from a typed
    -- 'Hedgehog.Lockstep.Observe.Observation' value, so you can write
    --
    -- > lsCmdObserve = runObservation ObserveEq
    --
    -- or
    --
    -- > lsCmdObserve = runObservation (ObserveProject normModel normReal)
    --
    -- instead of writing the assertion by hand.
  , lsCmdObserve :: modelOutput -> output -> Test ()

    -- | Additional invariants to check after each command, separate from
    -- the lockstep model-vs-real equality check in 'lsCmdObserve'.
    --
    -- Receives the post-state model and the real output. Use this for
    -- system invariants that hold regardless of the lockstep comparison
    -- (for example, "size never goes negative" or "no orphan handles").
    --
    -- Use @\\_ _ -> 'pure' ()@ if you have nothing extra to check.
  , lsCmdInvariants :: model -> output -> Test ()

    -- | Optional per-step coverage tags. Receives the pre-step model state,
    -- the post-step model state, and the model's predicted output. Returns
    -- a list of string tags; each tag becomes a 'Hedgehog.label' call so the
    -- test summary reports per-tag distribution.
    --
    -- This is the 'hedgehog-lockstep' analogue of @quickcheck-lockstep@'s
    -- @tagStep@. Calling 'Hedgehog.label' or 'Hedgehog.classify' inside
    -- 'lsCmdObserve' also works for ad-hoc coverage, but 'lsCmdTag' is the
    -- natural place for tags whose value depends on how the model state
    -- changed during this step (e.g., \"insert into empty\",
    -- \"delete present key\", \"size grew past threshold\").
    --
    -- Use @\\_ _ _ -> []@ if you don't need coverage tagging.
  , lsCmdTag :: model -> model -> modelOutput -> [String]
  }

-- | Convert a t'LockstepCmd' into a Hedgehog
-- t'Hedgehog.Internal.State.Command'.
--
-- The resulting command's @Ensure@ callback always 'Hedgehog.footnote's
-- the post-step model. Hedgehog only displays footnotes when a test
-- fails, so this gives a free post-mortem trace of the model state
-- evolution while costing nothing for passing tests. This is the
-- analogue of @quickcheck-lockstep@'s @monitoring@ counterexample
-- enrichment.
toLockstepCommand
  :: (Show model, Monad m)
  => LockstepCmd m model -> Command Gen m (LockstepState model)
{-# INLINABLE toLockstepCommand #-}
toLockstepCommand (LockstepCmd{..}) = Command gen exec callbacks
  where
    gen st = lsCmdGen st

    exec = lsCmdExec

    callbacks =
      [ Require $ \st input ->
          lsCmdRequire (lsModel st) input

      , Update $ \st input var ->
          let (modelOut, model') = lsCmdModel st input
          in insertModelResult var modelOut (st { lsModel = model' })

      , Ensure $ \oldSt newSt _input output -> do
          -- Model-state footnote: only displayed on failure, so this is
          -- effectively free for passing tests but gives a state trace
          -- when something goes wrong.
          footnote $ "model post-step: " <> show (lsModel newSt)
          -- The most recently inserted model result corresponds to
          -- this action. 'lsLastEntry' is set by 'insertModelResult'
          -- in the matching 'Update' callback above.
          case lsLastEntry newSt of
            Just dyn ->
              case fromDynamic dyn of
                Just modelOut -> do
                  -- Coverage tagging fires before observation so that
                  -- the test report records the tag even on the test
                  -- run that shrinks down to the failing example.
                  mapM_ (label . fromString) $
                    lsCmdTag (lsModel oldSt) (lsModel newSt) modelOut
                  lsCmdObserve modelOut output
                  lsCmdInvariants (lsModel newSt) output
                Nothing -> do
                  footnote $ unlines
                    [ "hedgehog-lockstep internal error: model result type mismatch."
                    , "  stored Dynamic type: " <> show (dynTypeRep dyn)
                    , "  expected:            " <> show (expectedModelType lsCmdObserve)
                    , "This indicates the Update and Ensure callbacks for one"
                    , "command saw inconsistent types, which should be impossible."
                    ]
                  failure
            Nothing -> do
              -- Unreachable: Hedgehog runs Update before Ensure for the
              -- same command, and Update always sets 'lsLastEntry' via
              -- 'insertModelResult'.
              footnote "hedgehog-lockstep internal error: Ensure ran without a preceding Update"
              failure
      ]

-- | Recover the model-output 'TypeRep' from a 'lsCmdObserve' function,
-- so that internal-error messages can name the type the 'Ensure' callback
-- expected to find in the stored 'Data.Dynamic.Dynamic'.
expectedModelType
  :: forall modelOutput output. Typeable modelOutput
  => (modelOutput -> output -> Test ()) -> TypeRep
expectedModelType _ = typeRep (Proxy @modelOutput)

-- | Transform the monad of a t'LockstepCmd' by lifting the exec function
-- through a natural transformation @m -> n@.
--
-- This is the hook that lets users write commands in a monad of their
-- choice (e.g., @'Control.Monad.Reader.ReaderT' Connection ('Hedgehog.PropertyT' 'IO')@)
-- while still running them with hedgehog's runner, which expects
-- @'Hedgehog.PropertyT' 'IO'@. The monad-parameterized runners
-- ('Hedgehog.Lockstep.Property.lockstepPropertyM' and friends) use
-- 'hoistLockstepCmd' under the hood.
--
-- All other fields are preserved unchanged.
hoistLockstepCmd
  :: (forall a. m a -> n a)
  -> LockstepCmd m model
  -> LockstepCmd n model
hoistLockstepCmd nat LockstepCmd{..} = LockstepCmd
  { lsCmdGen        = lsCmdGen
  , lsCmdExec       = nat . lsCmdExec
  , lsCmdModel      = lsCmdModel
  , lsCmdRequire    = lsCmdRequire
  , lsCmdObserve    = lsCmdObserve
  , lsCmdInvariants = lsCmdInvariants
  , lsCmdTag        = lsCmdTag
  }
{-# INLINABLE hoistLockstepCmd #-}
