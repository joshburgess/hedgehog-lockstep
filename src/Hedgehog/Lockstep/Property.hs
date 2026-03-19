{-# LANGUAGE RankNTypes #-}
module Hedgehog.Lockstep.Property
  ( lockstepProperty
  , lockstepPropertyWith
  , lockstepParallel
  , lockstepCommands
  ) where

import Hedgehog
  ( Gen
  , Property
  , PropertyT
  , property
  , forAll
  , evalIO
  , executeSequential
  , executeParallel
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Hedgehog.Internal.State (Command)

import Hedgehog.Lockstep.State (LockstepState, initialLockstepState)
import Hedgehog.Lockstep.Command (LockstepCmd, toLockstepCommand)

-- | Convert a list of 'LockstepCmd's into Hedgehog 'Command's.
lockstepCommands
  :: (Monad m)
  => [LockstepCmd m model] -> [Command Gen m (LockstepState model)]
lockstepCommands = map toLockstepCommand

-- | Run a sequential lockstep property test (pure commands).
--
-- For commands that need IO resources (e.g., @IORef@), use
-- 'lockstepPropertyWith' instead.
lockstepProperty
  :: Show model
  => model
  -> Int
  -> [LockstepCmd (PropertyT IO) model]
  -> Property
lockstepProperty model0 maxActions cmds = property $ do
  let commands = lockstepCommands cmds
  actions <- forAll $
    Gen.sequential (Range.linear 1 maxActions) (initialLockstepState model0) commands
  executeSequential (initialLockstepState model0) actions

-- | Run a sequential lockstep property test with IO-based resource setup.
--
-- The @IO env@ action creates a fresh resource before generation.
-- The @env -> IO ()@ action resets the resource before execution
-- (needed because hedgehog reuses IO effects across shrink attempts).
-- The @env -> [LockstepCmd ...]@ function creates commands using the resource.
lockstepPropertyWith
  :: Show model
  => model
  -> Int
  -> IO env
  -- ^ Create resource (runs once per test case)
  -> (env -> IO ())
  -- ^ Reset resource (runs before each execution, including shrink attempts)
  -> (env -> [LockstepCmd (PropertyT IO) model])
  -- ^ Commands using the resource
  -> Property
lockstepPropertyWith model0 maxActions setup reset mkCmds = property $ do
  env <- evalIO setup
  let commands = lockstepCommands (mkCmds env)
  actions <- forAll $
    Gen.sequential (Range.linear 1 maxActions) (initialLockstepState model0) commands
  evalIO (reset env)
  executeSequential (initialLockstepState model0) actions

-- | Run a parallel lockstep property test for linearizability.
--
-- Commands run in @PropertyT IO@. Use 'Hedgehog.evalIO' to lift
-- @IO@ actions inside 'lsCmdExec'.
lockstepParallel
  :: Show model
  => model
  -> Int
  -> Int
  -> [LockstepCmd (PropertyT IO) model]
  -> Property
lockstepParallel model0 maxPrefix maxBranch cmds = property $ do
  let commands = lockstepCommands cmds
  actions <- forAll $
    Gen.parallel (Range.linear 1 maxPrefix) (Range.linear 1 maxBranch)
      (initialLockstepState model0) commands
  executeParallel (initialLockstepState model0) actions
