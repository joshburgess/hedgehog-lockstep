{-# LANGUAGE RankNTypes #-}
-- | Property runners that turn a list of 'Hedgehog.Lockstep.Command.LockstepCmd'
-- values into a Hedgehog 'Hedgehog.Property'.
--
-- The @With@ variants set up an IO resource (an 'Data.IORef.IORef', a
-- database handle, etc.) that the commands use; the bare variants are for
-- commands that don't need one. The parallel variants generate concurrent
-- suffixes and check linearizability via Hedgehog's @executeParallel@.
module Hedgehog.Lockstep.Property
  ( lockstepProperty
  , lockstepPropertyWith
  , lockstepParallel
  , lockstepParallelWith
  , lockstepPropertyM
  , lockstepPropertyWithM
  , lockstepParallelM
  , lockstepParallelWithM
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
import Hedgehog.Lockstep.Command (LockstepCmd, hoistLockstepCmd, toLockstepCommand)

-- | Convert a list of 'LockstepCmd's into Hedgehog
-- 'Hedgehog.Internal.State.Command's.
lockstepCommands
  :: (Show model, Monad m)
  => [LockstepCmd m model] -> [Command Gen m (LockstepState model)]
lockstepCommands = map toLockstepCommand
{-# INLINABLE lockstepCommands #-}

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
{-# INLINABLE lockstepProperty #-}

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
{-# INLINABLE lockstepPropertyWith #-}

-- | Run a parallel lockstep property test for linearizability.
--
-- Commands run in @PropertyT IO@. Use 'Hedgehog.evalIO' to lift
-- @IO@ actions inside @lsCmdExec@.
--
-- For tests that need an IO resource (the common case), use
-- 'lockstepParallelWith'.
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
{-# INLINABLE lockstepParallel #-}

-- | Run a parallel lockstep property test with IO-based resource setup.
--
-- Like 'lockstepPropertyWith' but uses 'Gen.parallel' and 'executeParallel'
-- to test linearizability of concurrent operations.
--
-- The @IO env@ action creates the resource before generation. The reset
-- callback runs before execution (per test case). The commands must be
-- thread-safe: concurrent 'Data.IORef.modifyIORef'' is not safe, for
-- example; use 'Data.IORef.atomicModifyIORef'',
-- 'Control.Concurrent.MVar.MVar', or 'Control.Concurrent.STM.TVar' instead.
lockstepParallelWith
  :: Show model
  => model
  -> Int
  -- ^ Max actions in the sequential prefix
  -> Int
  -- ^ Max actions per parallel branch
  -> IO env
  -- ^ Create resource (runs once per test case)
  -> (env -> IO ())
  -- ^ Reset resource (runs before each execution, including shrink attempts)
  -> (env -> [LockstepCmd (PropertyT IO) model])
  -- ^ Commands using the resource
  -> Property
lockstepParallelWith model0 maxPrefix maxBranch setup reset mkCmds = property $ do
  env <- evalIO setup
  let commands = lockstepCommands (mkCmds env)
  actions <- forAll $
    Gen.parallel (Range.linear 1 maxPrefix) (Range.linear 1 maxBranch)
      (initialLockstepState model0) commands
  evalIO (reset env)
  executeParallel (initialLockstepState model0) actions
{-# INLINABLE lockstepParallelWith #-}

-- | Sequential lockstep test for commands written in an arbitrary monad
-- @m@.
--
-- The @runM@ argument is a natural transformation that lowers the user's
-- monad to @'PropertyT' 'IO'@ (the monad hedgehog's runner requires). For
-- a @'Control.Monad.Reader.ReaderT' env ('PropertyT' 'IO')@ user-monad,
-- @runM = (\\m -> 'Control.Monad.Reader.runReaderT' m env)@; for a
-- newtype around @'PropertyT' 'IO'@, it's just the unwrapper.
--
-- This is the hedgehog-lockstep analogue of @quickcheck-lockstep@'s
-- @runActions@-with-bracket pattern: write commands once in your natural
-- monad, then thread the runner separately.
lockstepPropertyM
  :: forall m model.
     (Show model, Monad m)
  => model
  -> Int
  -> (forall a. m a -> PropertyT IO a)
  -- ^ Lower the user's monad to @'PropertyT' 'IO'@
  -> [LockstepCmd m model]
  -> Property
lockstepPropertyM model0 maxActions runM cmds = property $ do
  let commands = lockstepCommands $ map (hoistLockstepCmd runM) cmds
  actions <- forAll $
    Gen.sequential (Range.linear 1 maxActions) (initialLockstepState model0) commands
  executeSequential (initialLockstepState model0) actions
{-# INLINABLE lockstepPropertyM #-}

-- | Sequential lockstep test for commands in an arbitrary monad with an
-- IO resource.
--
-- The @runM@ callback receives the live resource and a user-monad
-- computation, and lowers it to @'PropertyT' 'IO'@. For a typical
-- @'Control.Monad.Reader.ReaderT' env ('PropertyT' 'IO')@ stack with
-- environment @env@, pass @(\\env m -> 'Control.Monad.Reader.runReaderT' m env)@.
lockstepPropertyWithM
  :: forall env m model.
     (Show model, Monad m)
  => model
  -> Int
  -> IO env
  -- ^ Create resource (runs once per test case)
  -> (env -> IO ())
  -- ^ Reset resource (runs before each execution, including shrink attempts)
  -> (forall a. env -> m a -> PropertyT IO a)
  -- ^ Lower the user's monad given the resource
  -> (env -> [LockstepCmd m model])
  -- ^ Commands using the resource, in the user's monad
  -> Property
lockstepPropertyWithM model0 maxActions setup reset runM mkCmds = property $ do
  env <- evalIO setup
  let commands = lockstepCommands $ map (hoistLockstepCmd (runM env)) (mkCmds env)
  actions <- forAll $
    Gen.sequential (Range.linear 1 maxActions) (initialLockstepState model0) commands
  evalIO (reset env)
  executeSequential (initialLockstepState model0) actions
{-# INLINABLE lockstepPropertyWithM #-}

-- | Parallel lockstep test for commands in an arbitrary monad.
--
-- See 'lockstepPropertyM' for the @runM@ contract; concurrency safety
-- requirements are the same as 'lockstepParallel'.
lockstepParallelM
  :: forall m model.
     (Show model, Monad m)
  => model
  -> Int
  -> Int
  -> (forall a. m a -> PropertyT IO a)
  -> [LockstepCmd m model]
  -> Property
lockstepParallelM model0 maxPrefix maxBranch runM cmds = property $ do
  let commands = lockstepCommands $ map (hoistLockstepCmd runM) cmds
  actions <- forAll $
    Gen.parallel (Range.linear 1 maxPrefix) (Range.linear 1 maxBranch)
      (initialLockstepState model0) commands
  executeParallel (initialLockstepState model0) actions
{-# INLINABLE lockstepParallelM #-}

-- | Parallel lockstep test for commands in an arbitrary monad with an
-- IO resource.
--
-- See 'lockstepPropertyWithM' for the @runM@ contract; concurrency safety
-- requirements are the same as 'lockstepParallelWith'.
lockstepParallelWithM
  :: forall env m model.
     (Show model, Monad m)
  => model
  -> Int
  -- ^ Max actions in the sequential prefix
  -> Int
  -- ^ Max actions per parallel branch
  -> IO env
  -- ^ Create resource (runs once per test case)
  -> (env -> IO ())
  -- ^ Reset resource (runs before each execution, including shrink attempts)
  -> (forall a. env -> m a -> PropertyT IO a)
  -- ^ Lower the user's monad given the resource
  -> (env -> [LockstepCmd m model])
  -- ^ Commands using the resource, in the user's monad
  -> Property
lockstepParallelWithM model0 maxPrefix maxBranch setup reset runM mkCmds = property $ do
  env <- evalIO setup
  let commands = lockstepCommands $ map (hoistLockstepCmd (runM env)) (mkCmds env)
  actions <- forAll $
    Gen.parallel (Range.linear 1 maxPrefix) (Range.linear 1 maxBranch)
      (initialLockstepState model0) commands
  evalIO (reset env)
  executeParallel (initialLockstepState model0) actions
{-# INLINABLE lockstepParallelWithM #-}
