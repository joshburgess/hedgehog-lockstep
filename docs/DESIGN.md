# Reimagining `quickcheck-lockstep` with Hedgehog

This document has two parts. Sections 1 and 2 give the background: what
`quickcheck-lockstep` is, how Hedgehog differs, and why a port is interesting.
Sections 3 and 4 describe what `hedgehog-lockstep` actually ships today.
Section 5 lists ideas that were explored but deliberately deferred.

## 1. What `quickcheck-lockstep` Is and How It Works

### Core Philosophy

`quickcheck-lockstep` (by Edsko de Vries, Well-Typed) is a library for **lockstep-style testing** of stateful APIs built on top of `quickcheck-dynamic`. The fundamental idea is:

1. **Reify** a stateful API (database, file system, etc.) as a GADT with one constructor per API call.
2. Write two interpreters: one against the **real system** (blackbox), one against a pure **model**.
3. Generate random sequences of API calls, execute them against both, and compare responses **up to observability**.

The "lockstep" part means: at every step, the model and the system must agree on what the response was, modulo things we can't observe (e.g., opaque handles).

### Architecture Stack

```
┌──────────────────────────────────────┐
│          User's Test Code            │
│  (Action GADT, Model, Interpreters)  │
├──────────────────────────────────────┤
│        quickcheck-lockstep           │
│  InLockstep class                    │
│  ModelValue / Observable types       │
│  GVar abstraction                    │
│  Default StateModel methods          │
├──────────────────────────────────────┤
│        quickcheck-dynamic            │
│  StateModel class                    │
│  RunModel class                      │
│  Actions generation & shrinking      │
│  Variable tracking (Var)             │
├──────────────────────────────────────┤
│           QuickCheck                 │
│  Gen, Arbitrary, Property            │
│  Manual shrinking                    │
└──────────────────────────────────────┘
```

### Key Type Classes and Data Types

**`InLockstep state`** is the central class users implement:
- `data ModelValue state a`: GADT mapping real types to model types (e.g., `IO.Handle → MHandle`)
- `data Observable state a`: what we can actually compare (handles become unobservable)
- `observeModel :: ModelValue state a → Observable state a`
- `modelNextState :: action → lookUp → state → (ModelValue state a, state)`
- `usedVars :: action → [AnyGVar (ModelOp state)]`
- `arbitraryWithVars :: ModelFindVariables state → state → Gen (Any action)`
- `shrinkWithVars :: ModelFindVariables state → state → action → [Any action]`
- `tagStep :: (state, state) → action → ModelValue state a → [String]`

**`RunLockstep state m`** is the subclass for execution:
- `observeReal :: Proxy m → action → Realized m a → Observable state a`

**`GVar op f`** is a generalized variable with a Functor-like structure:
```haskell
data GVar op f where
  GVar :: Typeable x => Var x -> op x y -> GVar op y
```
This solves the critical problem that `quickcheck-dynamic` gives exactly **one variable per action**. If `Open` returns `Either Err (Handle, FilePath)`, you need to extract just the `Handle` for a subsequent `Close`. The `Op` DSL (`OpFst`, `OpSnd`, `OpLeft`, `OpRight`, `OpComp`) lets you "map" over variables.

**`Lockstep state`** is an opaque wrapper holding the model state plus a model-side variable environment:
```haskell
data Lockstep state = Lockstep {
    lockstepModel :: state,
    lockstepEnv   :: EnvF (ModelValue state)
  }
```

### How a Test Executes

1. **Generation**: `arbitraryWithVars` generates actions given the current model state and available variables (found via `varsOfType`). Variables are `GVar`s with composed `Op`s.
2. **Stepping**: `nextState` runs `modelNextState` to get the model response, then stores it in `lockstepEnv`.
3. **Execution**: `perform` (from `RunModel`) runs the action against the real system.
4. **Postcondition**: Both results are projected through `observeModel`/`observeReal` to `Observable` values, then compared for equality.
5. **Shrinking**: `quickcheck-dynamic` shrinks the action list; the default precondition ensures all `usedVars` remain defined after removals.
6. **Monitoring/Tagging**: `tagStep` labels each step; `tagActions` re-runs the model to collect tags for `labelledExamples`.

### Pain Points in the Current Design

1. **Three-layer dependency**: users must understand quickcheck-lockstep, quickcheck-dynamic, AND QuickCheck.
2. **Boilerplate**: `ModelValue`, `Observable`, `observeModel`, `observeReal`, `usedVars`, `InterpretOp` instances, all required per test.
3. **Manual shrinking**: QuickCheck's approach requires `shrinkWithVars`. If you forget, no shrinking happens.
4. **Single variable per action**: the `GVar`/`Op` machinery is clever but adds conceptual overhead. Users must understand Coyoneda-like structures to write tests.
5. **`Realized` type family**: confusing for newcomers; it's essentially `a` for `IO`-based tests.
6. **No parallel testing**: `quickcheck-dynamic` lacks parallel execution (unlike `quickcheck-state-machine` or Hedgehog).
7. **Labelling is re-execution**: `tagActions` must re-run the entire model because `Actions` doesn't record responses (unlike `Commands` in qsm).

---

## 2. Hedgehog's State Machine Testing: Key Differences

### Symbolic vs Concrete Variables
Hedgehog parameterises state and inputs by a type constructor `v :: Type → Type`:
- During generation: `v ~ Symbolic` (opaque placeholders)
- During execution: `v ~ Concrete` (real values)

The state type is `state :: (Type → Type) → Type`, so `state Symbolic` and `state Concrete` are different types.

### Command-Based Architecture
```haskell
data Command gen m state = Command {
    commandGen     :: state Symbolic → Maybe (gen (input Symbolic)),
    commandExecute :: input Concrete → m output,
    commandCallbacks :: [Callback input output state]
  }

data Callback input output state
  = Require (state Symbolic → input Symbolic → Bool)
  | Update (∀ v. Ord1 v ⇒ state v → input v → Var output v → state v)
  | Ensure (state Concrete → state Concrete → input Concrete → output → Test ())
```

Each command is a self-contained record. There's no single GADT; commands are a *list* of `Command` values.

### Integrated Shrinking
Hedgehog uses tree-based generators where shrink candidates are embedded in the generation tree. No separate `shrink` function is needed. This is a major ergonomic win.

### HTraversable / TraversableB
Hedgehog requires a traversable instance for input types to substitute `Symbolic`-phase variables with `Concrete` ones. This is Hedgehog's counterpart to lockstep's `usedVars`. `hedgehog-lockstep` uses barbies' `FunctorB` and `TraversableB` to satisfy this in a way that composes with GVars.

### Parallel Testing
Hedgehog has `executeParallel` for linearizability testing, something neither `quickcheck-dynamic` nor `quickcheck-lockstep` currently provides.

---

## 3. What `hedgehog-lockstep` Actually Ships

The implementation is deliberately narrow: a single `LockstepCmd` record that
wraps a Hedgehog `Command`, plus enough machinery to store model-side results
in the state and project values out of compound return types.

### 3.1 Design Goals

1. **Preserve the lockstep philosophy**: model and system must agree on responses up to observability.
2. **Leverage integrated shrinking**: eliminate `shrinkWithVars` entirely.
3. **Reduce boilerplate**: fewer associated types, less manual wiring.
4. **Support parallel testing**: inherit Hedgehog's `executeParallel`.
5. **Keep the `GVar` insight**: allow projections from composite return types.

### 3.2 The `LockstepCmd` record

```haskell
data LockstepCmd m model = forall input output modelOutput.
  ( TraversableB input, Show (input Symbolic)
  , Show output, Typeable output, Ord output
  , Typeable modelOutput, Eq modelOutput, Show modelOutput
  ) => LockstepCmd
  { lsCmdGen        :: LockstepState model Symbolic -> Maybe (Gen (input Symbolic))
  , lsCmdExec       :: input Concrete -> m output
  , lsCmdModel      :: forall v. Ord1 v =>
                       LockstepState model v -> input v -> (modelOutput, model)
  , lsCmdRequire    :: model -> input Symbolic -> Bool
  , lsCmdObserve    :: modelOutput -> output -> Test ()
  , lsCmdInvariants :: model -> output -> Test ()
  , lsCmdTag        :: model -> model -> modelOutput -> [String]
  }
```

The library converts each `LockstepCmd` into a Hedgehog `Command` by wiring:

- **Require**: `lsCmdRequire` plus Hedgehog's automatic variable-definedness check.
- **Update**: run `lsCmdModel` to advance the model, then store the model-side
  result in `LockstepState` keyed by the Hedgehog `Var`.
- **Ensure**: retrieve the stored model result and compare it with the real
  output via `lsCmdObserve`. The Ensure callback also runs `lsCmdInvariants`
  for system invariants, calls `lsCmdTag` and feeds each returned tag to
  `Hedgehog.label` for per-step coverage, and `footnote`s the post-step
  model state so that test failures display the model trail. Hedgehog only
  prints footnotes on failure, so passing tests pay nothing for the
  diagnostic.

This is simpler than the `InLockstep` typeclass stack in quickcheck-lockstep:
there's no `ModelValue` GADT, no `Observable` GADT, no `observeModel`/
`observeReal` pair, and no associated type families. The comparison between
model and real is expressed directly by the user in `lsCmdObserve`, using
Hedgehog assertions. Hedgehog's diff infrastructure handles pretty-printing.

The cost of this directness is that `hedgehog-lockstep` does not automatically
know how model and real types relate. The user expresses that relationship
inside `lsCmdModel` (by constructing `modelOutput`) and `lsCmdObserve` (by
deciding how to compare). For most cases this is straightforward and shorter
than the GADT-based alternative.

### 3.3 Model Environment and `LockstepState`

```haskell
data LockstepState model v = LockstepState
  { lsModel     :: !model
  , lsNextVarId :: !Int
  , lsEntries   :: !(ModelEnv v)
  , lsVars      :: !(Map TypeRep [SomeVar v])
  , lsLastEntry :: !(Maybe Dynamic)
  }

newtype ModelEnv v = ModelEnv (Map (VarKey v) Dynamic)
```

`lsEntries` is the lockstep environment: a `Map` keyed by an existential
`VarKey` that wraps a Hedgehog `Var` with phase-polymorphic ordering. Values
are model-side results stored as `Dynamic`. `lsVars` is bucketed by type so
`varsOfType` is a single `Map` lookup followed by a per-bucket fold rather
than a linear scan over every variable seen so far. `lsLastEntry` caches the
most recently inserted result so the `Ensure` callback can compare it with
the real output without re-scanning.

The environment supports both phases:

- In the **Symbolic** phase, variable comparison is by Hedgehog's stable name
  (unchanged by shrinking).
- In the **Concrete** phase, comparison is by value.

Both cases are unified through `Ord1` on the phase functor `v`. The `Ord`
instance on `VarKey` first compares type representations, then defers to
`Ord1` for same-typed variables.

Lookup is O(log n) in the number of completed actions. An earlier
implementation used a list and `Ord1`-based linear scan, which made the
per-test cost quadratic on long action traces.

### 3.4 `GVar` and `Op`: Projections from Compound Results

When a command returns a compound type like `Either String (Handle, Name)`,
a later command may need just the `Handle`. `GVar` and `Op` handle this:

```haskell
data GVar a v where
  GVar :: (Typeable x, Ord x)
       => !(Var x v) -> !String -> !(Dynamic -> Maybe a) -> GVar a v

data Op a b where
  OpId    :: Op a a
  OpFst   :: Op (a, b) a
  OpSnd   :: Op (a, b) b
  OpLeft  :: Op (Either a b) a
  OpRight :: Op (Either a b) b
  OpComp  :: !(Op b c) -> !(Op a b) -> Op a c
```

`GVar` bundles:

1. the underlying Hedgehog `Var`, so the framework can track which variables
   the input references and keep them defined during shrinking;
2. a human-readable label for debugging; and
3. a resolution closure that projects a value out of the `Dynamic` model
   result.

`Op`-based projections are partial: applying `OpLeft` to a `Right` returns
`Nothing`. That's exactly the behaviour callers need when a `GVar` is
generated against a symbolic variable whose concrete outcome turns out to not
match the projection.

The op set is extensible. `InterpretOp` is a class with one method:

```haskell
class InterpretOp op where
  interpretOp :: op a b -> a -> Maybe b
```

Users who need projections beyond pair/sum (list head, record fields, custom
decoders) define their own GADT, write an `InterpretOp` instance and a `Show`
instance, and pass it to `mkGVar` or `mapGVar` exactly like `Op`. The
built-in `Op` is one instance among many; the library does not commit users
to its constructor set. This mirrors `quickcheck-lockstep`'s
`Operation`/`InterpretOp` design.

The original design considered storing projected model values directly in
`LockstepState` (Option A in an earlier draft). The `GVar`-based Option B was
chosen because it keeps the state type small and mirrors the
`quickcheck-lockstep` API, minimising the conceptual shift for users moving
over.

### 3.5 Eliminating Manual Shrinking

This is the biggest ergonomic win. With Hedgehog:

- Action *argument* shrinking is integrated (strings shrink, ints shrink, etc.).
- Action *sequence* shrinking is handled by Hedgehog's `Gen.sequential` and
  `Gen.parallel` combinators.
- `shrinkWithVars` is gone entirely.

The model environment (`lsEntries`) is keyed by `Var` identity, so after
shrinking removes a command, the remaining `GVar`s still resolve correctly
against the updated environment. No user code changes when Hedgehog shrinks.

### 3.6 Parallel Testing

`lockstepParallel` wraps Hedgehog's `executeParallel`:

```haskell
lockstepParallel
  :: Show model
  => model -> Int -> Int
  -> [LockstepCmd (PropertyT IO) model]
  -> Property
```

For tests that need an IO resource (the common case),
`lockstepParallelWith` takes a setup plus a reset callback, mirroring
`lockstepPropertyWith`. Reset is important here because Hedgehog retries
parallel runs to distinguish genuine non-linearisability from scheduling
flakiness, and each retry must start from a fresh resource state.

This enables linearizability testing: checking that concurrent executions are
equivalent to *some* sequential ordering. `quickcheck-lockstep` does not
provide this.

---

## 4. Actual API Surface

```haskell
module Hedgehog.Lockstep
  ( -- Commands
    LockstepCmd (..)
  , toLockstepCommand
  , lockstepCommands

    -- State
  , LockstepState (..)
  , ModelEnv
  , initialLockstepState
  , getModel
  , getEntries
  , varsOfType

    -- Generalized variables
  , GVar (..)
  , mkGVar
  , mkGVarId
  , resolveGVar
  , concreteGVar
  , gvarLabel

    -- Structural projections
  , Op (..)
  , applyOp
  , (>>>)

    -- Running tests
  , lockstepProperty
  , lockstepPropertyWith
  , lockstepParallel
  , lockstepParallelWith

    -- Re-exports from Hedgehog and barbies
  , Var (..), Symbolic, Concrete
  , Gen, Test, Property, PropertyT, (===)
  , FunctorB (..), TraversableB (..)
  )
```

There is no `Observe` class, no `ModelResult` type family, and no
`lockstepLabelledExamples`. Sections 1-2 describe the shape those would have
taken; section 5 explains why they were deferred.

---

## 5. Ideas Explored but Deferred

The items below were considered during design. Each was dropped or postponed
because the simpler API already handles the common case, or because the
feature needs real-world use to guide its shape.

### 5.1 `ModelResult` type family and `Observe` class

The earlier draft proposed a type family `ModelResult state a` and a class
`Observe state a` to automate the model-to-real comparison. The shipped
API does not have a class. Instead, two layers cover the same ground:

* `lsCmdObserve :: modelOutput -> output -> Test ()` is the per-command
  comparison hook. It is the universal escape hatch.
* `Observation modelOutput output` is a typed DSL that captures the
  common patterns (`ObserveEq`, `ObserveProject`, `ObservePair`,
  `ObserveCustom`); `runObservation` turns one into the function
  `lsCmdObserve` expects. This is the hedgehog-lockstep analogue of
  `quickcheck-lockstep`'s `Observable`/`ModelValue` split, expressed as
  data instead of a class so it composes per-command without
  orphan-instance hazards.

A class-based design (with associated types pinning the model side to
the real side) is still a possible future direction once there's enough
real-world usage to see which comparison patterns recur often enough to
justify giving up the per-command flexibility.

### 5.2 Automatic `usedVars` via `HTraversable`

Hedgehog already traverses inputs to substitute symbolic variables with
concrete ones. `hedgehog-lockstep` relies on the user's
`FunctorB`/`TraversableB` instances for that, which is the same information
`usedVars` would encode. No separate API surface is needed, so this "feature"
is really a property of the design rather than additional code.

### 5.3 Tagging and labelled examples

The library ships three layers of coverage support:

* `lsCmdTag :: model -> model -> modelOutput -> [String]`, the analogue of
  `quickcheck-lockstep`'s `tagStep`. It runs in the `Ensure` callback, sees
  the pre- and post-step model and the model's predicted output, and feeds
  each returned tag to `Hedgehog.label` so per-tag distribution shows up in
  the test summary.
* `Hedgehog.classify` and `Hedgehog.label` calls inside `lsCmdObserve` for
  ad-hoc tagging that depends on the real output.
* `lockstepLabelledExamples`, the analogue of `QuickCheck`'s
  `labelledExamples` and `quickcheck-lockstep`'s `tagActions`. It samples
  random command sequences, runs only the model side, accumulates tags
  from `lsCmdTag`, and prints one shortest trace per tag. Built directly
  on top of Hedgehog's `Gen` rather than relying on a `labelledExamples`
  primitive (Hedgehog has none): we just sample model traces ourselves
  and aggregate.

### 5.4 Compositional commands

`combineCommands :: [LockstepCmd m stateA] -> [LockstepCmd m stateB]
                -> [LockstepCmd m (stateA, stateB)]` would let two subsystems
share a backend. The mechanical implementation is straightforward but the
`Ord1` and `Typeable` plumbing across the combined state deserves more
thought than v0.1 warranted.

### 5.5 Extra per-command `Ensure` callbacks

The library ships `lsCmdInvariants :: model -> output -> Test ()` for system
invariants that hold after every command, separate from the lockstep
model-vs-real equality check in `lsCmdObserve` ("after every Write, the file
size is positive"; "the open-handle count never exceeds the next-handle
counter"). The `Test.HandleStore` and `Test.KVStore` examples exercise this.

### 5.6 `falsify` as an alternative foundation

`falsify` (also by Edsko de Vries) has internal shrinking that works cleanly
across monadic bind, which is a better theoretical fit for stateful testing
than Hedgehog's integrated shrinking. Hedgehog was chosen for v0.1 because it
has a mature `executeParallel` and a larger ecosystem. A `falsify-lockstep`
peer would be interesting future work.

---

## 6. Summary: What Changes, What Stays

| Aspect | quickcheck-lockstep | hedgehog-lockstep |
|--------|-------------------|-------------------|
| **Shrinking** | Manual (`shrinkWithVars`) | Integrated (free) |
| **Variables** | `GVar` with `Op` DSL, extensible via `Operation`/`InterpretOp` | `GVar` adapted for `Symbolic`/`Concrete`, extensible via `InterpretOp` |
| **Model / real divergence** | `ModelValue`/`Observable` GADTs plus `observeModel`/`observeReal` | `modelOutput` existential plus per-command `lsCmdObserve`, with `Observation` GADT (`ObserveEq`/`ObserveProject`/`ObservePair`/`ObserveCustom`) for structured cases |
| **Variable tracking** | Manual `usedVars` | Automatic via `TraversableB` |
| **Parallel testing** | Not supported | Supported via `executeParallel` |
| **Tagging** | `tagStep` + `tagActions` re-runs model | `lsCmdTag` + Hedgehog `label`; ad-hoc `classify` / `label` in observer also supported |
| **Error messages** | Custom `Show` for `Observable` | Hedgehog's diff infrastructure |
| **Dependency stack** | QC + qc-dynamic + qc-lockstep | Hedgehog + hedgehog-lockstep |
| **Command structure** | Single GADT + typeclass | List of `Command` records |

### What's Lost

- **Dynamic logic**: `quickcheck-dynamic` supports DL specifications; Hedgehog does not.
- **Single GADT elegance**: the single `Action` GADT in lockstep is arguably more elegant than a list of `Command` records. Pattern matching on a closed GADT gives exhaustiveness checking.

### What's Gained

- **No manual shrinking**: the single biggest ergonomic improvement.
- **Parallel testing**: linearizability checking for free.
- **Less boilerplate**: fewer associated types, no `ModelValue`/`Observable` GADTs.
- **Better error output**: Hedgehog's pretty-printing and diff support.
- **Simpler mental model**: two layers instead of three.
