# Changelog

All notable changes to `hedgehog-lockstep` will be documented in this file.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Haskell PVP](https://pvp.haskell.org/).

## 0.1.0.0

Initial release.

### Added

- `LockstepCmd` record bundling an API operation with its model
  interpretation and observation.
- `LockstepState` wrapping a user-defined model plus a model environment
  keyed by Hedgehog `Var` identity.
- `GVar` and `Op` (`OpId`, `OpFst`, `OpSnd`, `OpLeft`, `OpRight`, `OpComp`)
  for projecting values out of compound action outputs.
- `varsOfType`, `mkGVar`, `mkGVarId`, `resolveGVar`, `concreteGVar`,
  `gvarLabel` for enumerating and resolving generalized variables.
- Four test entry points:
  `lockstepProperty`, `lockstepPropertyWith`, `lockstepParallel`, and
  `lockstepParallelWith`.
- `lockstepCommands` for users who want to drive
  `Gen.sequential` / `executeSequential` directly.
- README guidance and a `Test.KVStore` example for using
  `Hedgehog.label` / `classify` inside `lsCmdObserve` for coverage labels.
- `lsCmdInvariants :: model -> output -> Test ()` field on `LockstepCmd`
  for system invariants that should hold after every command, separate
  from the lockstep equality check in `lsCmdObserve`.
- Module-level Haddock for every public module, plus disambiguated
  identifier references so Haddock builds cleanly with no warnings.
- README caveat documenting the dependency on `Hedgehog.Internal.State`.
- `Test.UnitCoverage` exercising `applyOp`, `gvarLabel`, and `mkGVarId`.
