{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Hedgehog.Lockstep.Command
  ( LockstepCmd (..)
  , toLockstepCommand
  ) where

import Data.Dynamic (fromDynamic)
import Data.Typeable (Typeable)
import Data.Functor.Barbie (TraversableB)
import Data.Functor.Classes (Ord1)
import Hedgehog (Gen, Test, footnote, failure)
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
    -- This runs in @forall v. Ord1 v@ context. Use 'resolveGVar' with
    -- 'getEntries' to resolve 'GVar's in the input to their model values.
  , lsCmdModel :: forall v. Ord1 v => LockstepState model v -> input v -> (modelOutput, model)

    -- | Additional preconditions beyond variable-definedness.
  , lsCmdRequire :: model -> input Symbolic -> Bool

    -- | Compare model output with real output.
    -- Use Hedgehog assertions ('===' etc.) to report mismatches.
  , lsCmdObserve :: modelOutput -> output -> Test ()
  }

-- | Convert a 'LockstepCmd' into a Hedgehog 'Command'.
toLockstepCommand
  :: (Monad m)
  => LockstepCmd m model -> Command Gen m (LockstepState model)
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

      , Ensure $ \_oldSt newSt _input output ->
          -- The most recent entry in newSt corresponds to this action
          case lsEntries newSt of
            (ModelEntry _ dyn : _) ->
              case fromDynamic dyn of
                Just modelOut -> lsCmdObserve modelOut output
                Nothing -> do
                  footnote "hedgehog-lockstep internal error: model result type mismatch"
                  failure
            [] -> do
              footnote "hedgehog-lockstep internal error: no model entries"
              failure
      ]
