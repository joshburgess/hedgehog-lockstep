# Changelog

All notable changes to `hedgehog-lockstep` will be documented in this file.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Haskell PVP](https://pvp.haskell.org/).

## 0.1.0.0

Initial release.

### Added

- `LockstepCmd` record bundling an API operation with its model
  interpretation and observation.
- `LockstepState` wrapping a user-defined model plus a `ModelEnv`,
  a `Map` keyed by Hedgehog `Var` identity (O(log n) lookup) using a
  phase-polymorphic `Ord1`-based comparison.
- `GVar` and `Op` (`OpId`, `OpFst`, `OpSnd`, `OpLeft`, `OpRight`, `OpComp`)
  for projecting values out of compound action outputs.
- `varsOfType`, `mkGVar`, `mkGVarId`, `mapGVar`, `resolveGVar`,
  `concreteGVar`, `gvarLabel` for enumerating and resolving generalized
  variables. `mapGVar` composes an additional `Op` projection onto an
  existing `GVar`, the analogue of `quickcheck-lockstep`'s `mapGVar`.
- Four test entry points:
  `lockstepProperty`, `lockstepPropertyWith`, `lockstepParallel`, and
  `lockstepParallelWith`.
- Monad-parameterized variants
  `lockstepPropertyM`, `lockstepPropertyWithM`, `lockstepParallelM`, and
  `lockstepParallelWithM` accepting a
  `forall a. m a -> PropertyT IO a` (or `env -> m a -> PropertyT IO a`)
  natural transformation. Lets users write commands in a custom monad
  stack (e.g., `ReaderT env (PropertyT IO)`) and supply the runner
  separately, without hardcoding `PropertyT IO` into every `lsCmdExec`.
- `hoistLockstepCmd :: (forall a. m a -> n a) -> LockstepCmd m model -> LockstepCmd n model`
  for retargeting a command's monad.
- New module `Hedgehog.Lockstep.Observe` exposing an `Observation`
  GADT (`ObserveEq`, `ObserveProject`, `ObservePair`, `ObserveCustom`)
  and `runObservation`. The GADT factors out the common
  model-output-versus-real-output comparison patterns into a typed DSL
  so users can write `lsCmdObserve = runObservation ObserveEq` or
  `lsCmdObserve = runObservation (ObserveProject normM normR)` instead
  of writing the assertion by hand. This is the hedgehog-lockstep
  analogue of `quickcheck-lockstep`'s `Observable`/`ModelValue` split.
- New module `Hedgehog.Lockstep.Examples` exposing
  `lockstepLabelledExamples`, a model-only driver that samples random
  command sequences, accumulates the tags emitted by `lsCmdTag`, and
  prints one shortest sampled trace per tag. This is the analogue of
  `QuickCheck`'s `labelledExamples` and `quickcheck-lockstep`'s
  `tagActions`: it answers "is the generator actually hitting the
  labelled cases I expect" without running the real system at all.
- `lockstepCommands` for users who want to drive
  `Gen.sequential` / `executeSequential` directly.
- README guidance and a `Test.KVStore` example for using
  `Hedgehog.label` / `classify` inside `lsCmdObserve` for coverage labels.
- `lsCmdInvariants :: model -> output -> Test ()` field on `LockstepCmd`
  for system invariants that should hold after every command, separate
  from the lockstep equality check in `lsCmdObserve`.
- `lsCmdTag :: model -> model -> modelOutput -> [String]` field on
  `LockstepCmd` for per-step coverage tagging. Receives the pre- and
  post-step model and the model's predicted output; each returned tag is
  reported via `Hedgehog.label`. This is the analogue of
  `quickcheck-lockstep`'s `tagStep` and is the natural place for tags
  whose value depends on how the model state changed during the step.
- Per-step model-state footnote: `toLockstepCommand` now adds a
  `footnote` of the post-step model after every command. Hedgehog only
  displays footnotes on test failure, so passing tests are unaffected,
  but a failure now shows the model-state evolution alongside the
  shrunken command sequence (analogue of `quickcheck-lockstep`'s
  `monitoring` counterexample enrichment).
- Module-level Haddock for every public module, plus disambiguated
  identifier references so Haddock builds cleanly with no warnings.
- README caveat documenting the dependency on `Hedgehog.Internal.State`.
- `Test.UnitCoverage` exercising `applyOp`, `gvarLabel`, `mapGVar`, and
  `mkGVarId`.
