# Reimagining `quickcheck-lockstep` with Hedgehog

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

**`InLockstep state`** — the central class users implement:
- `data ModelValue state a` — GADT mapping real types to model types (e.g., `IO.Handle → MHandle`)
- `data Observable state a` — what we can actually compare (handles become unobservable)
- `observeModel :: ModelValue state a → Observable state a`
- `modelNextState :: action → lookUp → state → (ModelValue state a, state)`
- `usedVars :: action → [AnyGVar (ModelOp state)]`
- `arbitraryWithVars :: ModelFindVariables state → state → Gen (Any action)`
- `shrinkWithVars :: ModelFindVariables state → state → action → [Any action]`
- `tagStep :: (state, state) → action → ModelValue state a → [String]`

**`RunLockstep state m`** — subclass for execution:
- `observeReal :: Proxy m → action → Realized m a → Observable state a`

**`GVar op f`** — generalized variables with a Functor-like structure:
```haskell
data GVar op f where
  GVar :: Typeable x => Var x -> op x y -> GVar op y
```
This solves the critical problem that `quickcheck-dynamic` gives exactly **one variable per action**. If `Open` returns `Either Err (Handle, FilePath)`, you need to extract just the `Handle` for a subsequent `Close`. The `Op` DSL (`OpFst`, `OpSnd`, `OpLeft`, `OpRight`, `OpComp`) lets you "map" over variables.

**`Lockstep state`** — opaque wrapper holding the model state + model-side variable environment:
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

1. **Three-layer dependency**: Users must understand quickcheck-lockstep, quickcheck-dynamic, AND QuickCheck.
2. **Boilerplate**: `ModelValue`, `Observable`, `observeModel`, `observeReal`, `usedVars`, `InterpretOp` instances — all required per test.
3. **Manual shrinking**: QuickCheck's approach requires `shrinkWithVars`. If you forget, no shrinking happens.
4. **Single variable per action**: The `GVar`/`Op` machinery is clever but adds conceptual overhead. Users must understand Coyoneda-like structures to write tests.
5. **`Realized` type family**: Confusing for newcomers; it's essentially `a` for `IO`-based tests.
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

Each command is a self-contained record. There's no single GADT — commands are a *list* of `Command` values.

### Integrated Shrinking
Hedgehog uses tree-based generators where shrink candidates are embedded in the generation tree. No separate `shrink` function is needed. This is a major ergonomic win.

### HTraversable
Hedgehog requires `HTraversable` instances for input types to substitute `Symbolic` → `Concrete` variables. This is the Hedgehog counterpart to lockstep's `usedVars`.

### Parallel Testing
Hedgehog has `executeParallel` for linearizability testing — something neither `quickcheck-dynamic` nor `quickcheck-lockstep` currently provides.

---

## 3. Designing `hedgehog-lockstep`

### Design Goals

1. **Preserve the lockstep philosophy**: model + system agree on responses up to observability.
2. **Leverage integrated shrinking**: eliminate `shrinkWithVars` entirely.
3. **Reduce boilerplate**: fewer associated types, less manual wiring.
4. **Support parallel testing**: inherit Hedgehog's `executeParallel`.
5. **Keep the `GVar` insight**: allow projections from composite return types.
6. **Improve tagging**: make it less costly than full model re-execution.

### Core Design

#### 3.1 The Model Value / Observable Problem

The `ModelValue` / `Observable` split exists because:
- The model may return a different type (mock handle vs real handle)
- Comparison needs a common type

In Hedgehog, we can simplify this. Since `Var output v` is already parameterised, we can track the mapping between real and model types at the *variable* level rather than through a separate GADT.

**Proposal**: Replace `ModelValue` + `Observable` with a single `ModelResult` type family and an `Observe` class:

```haskell
-- What the model returns for a given real return type
type family ModelResult state a

-- How to compare real and model results
class Observe state a where
  observe :: a -> ModelResult state a -> Maybe String
  -- Nothing = match, Just msg = mismatch description
```

For the handle case:
```haskell
type instance ModelResult FsState IO.Handle = MHandle
type instance ModelResult FsState String    = String
type instance ModelResult FsState ()        = ()

instance Observe FsState IO.Handle where
  observe _ _ = Nothing  -- handles are unobservable

instance Observe FsState String where
  observe real model
    | real == model = Nothing
    | otherwise     = Just $ show real ++ " /= " ++ show model
```

This eliminates the GADT boilerplate of `ModelValue`, `Observable`, `observeModel`, and `observeReal`.

#### 3.2 Rethinking Variables: `GVar` in Hedgehog's World

Hedgehog's `Var a v` is parameterised by the phase functor `v`. The `GVar` problem (extracting a component from a compound return type) still exists.

**Option A: Encode projections in the state**

Instead of `GVar`, store projected model values directly in the state during the `Update` callback:

```haskell
data FsState v = FsState {
    mockFs      :: Mock,
    openHandles :: Map (Var (Either Err (IO.Handle, File)) v)
                       MHandle,  -- model handle stored on success
    stats       :: Stats
  }
```

When `Open` succeeds, the `Update` callback stores the model handle. When `Close` needs a handle, it looks up the original `Var` in the map and retrieves the associated `MHandle`.

This is simpler than `GVar` but means the state carries more information.

**Option B: A Hedgehog-native `GVar`** (CHOSEN)

Define a generalized variable that works with Hedgehog's `Symbolic`/`Concrete` split:

```haskell
data GVar a v where
  GVar :: Typeable x => Var x v -> (x -> Maybe a) -> GVar a v

-- For generation (Symbolic phase), we only need the Var identity
-- For execution (Concrete phase), we apply the projection
resolveGVar :: GVar a Concrete -> Maybe a
resolveGVar (GVar (Concrete x) f) = f x
```

The problem: `GVar a Symbolic` can't apply `f` because there's no value yet. But this is fine — during generation we only need the variable's identity (for `Require` checks), and during execution we resolve it.

To make `GVar` work with `HTraversable`, we'd need:
```haskell
instance HTraversable (GVar a) where
  htraverse f (GVar var proj) = GVar <$> htraverse f var <*> pure proj
```

Wait — `Var` is already `HTraversable` in hedgehog, so this composes naturally.

We should **also** provide the `Op` DSL for `Show`/`Eq` on projections:
```haskell
data GVar' op a v where
  GVar' :: Typeable x => Var x v -> op x a -> GVar' op a v
```

This way projections are inspectable (for debugging/showing test cases).

#### 3.3 The Lockstep Command

Instead of having users write bare Hedgehog `Command`s, we provide a higher-level interface:

```haskell
data LockstepCmd gen m state = forall input output.
  (Show (input Symbolic), Typeable output, Observe state output) =>
  LockstepCmd {
    -- Generate an action given the model state
    lsCmdGen :: state Symbolic -> Maybe (gen (input Symbolic)),

    -- Run against the real system
    lsCmdExec :: input Concrete -> m output,

    -- Run against the model (pure)
    lsCmdModel :: state Symbolic -> input Symbolic
               -> (ModelResult state output, state Symbolic),

    -- Additional preconditions beyond variable-definedness
    lsCmdRequire :: state Symbolic -> input Symbolic -> Bool,

    -- Tag this step for labelling
    lsCmdTag :: state Symbolic -> input Symbolic
             -> ModelResult state output -> [String]
  }
```

The library then converts each `LockstepCmd` into a Hedgehog `Command` by:
1. **Require**: Check `lsCmdRequire` + all variables in input are defined
2. **Update**: Run `lsCmdModel` to step the model state, store result
3. **Ensure**: Compare `output` with `ModelResult state output` via `Observe`

#### 3.4 Eliminating Manual Shrinking

This is the biggest win. With Hedgehog:
- Action *argument* shrinking is integrated (strings shrink, ints shrink, etc.)
- Action *sequence* shrinking is handled by Hedgehog's `sequential`/`parallel` combinators
- **No `shrinkWithVars` needed**

The generator in `lsCmdGen` uses Hedgehog's `Gen` which carries its own shrink tree. When we pick variables from the state, we can use `Gen.element` which also shrinks (tries different available variables).

#### 3.5 Parallel Testing

With the `Command`-based architecture, `executeParallel` comes for free:

```haskell
prop_fsParallel :: Property
prop_fsParallel = property $ do
  actions <- forAll $
    Gen.parallel (Range.linear 1 50) (Range.linear 1 10)
      initialState lockstepCommands
  executeParallel initialState actions
```

This enables linearizability testing — checking that concurrent executions are equivalent to *some* sequential ordering. This is a capability lockstep-style testing can greatly benefit from.

#### 3.6 Improved Tagging

**Problem**: In `quickcheck-lockstep`, `tagActions` must re-execute the entire model to produce tags, because `Actions` doesn't store responses.

**Solution**: In our design, the `Update` callback stores model results in the state. After execution, we have the full history of `(state, input, modelResult)` triples. We can tag from this history directly.

```haskell
tagHistory :: [(state Concrete, SomeInput Concrete, SomeModelResult)] -> [String]
```

Even better: Hedgehog's `Ensure` callback sees `(oldState, newState, input, output)` — we can emit tags during execution and collect them, avoiding re-execution entirely.

---

## 4. Proposed API Surface

```haskell
module Hedgehog.Lockstep (
    -- Core type families
    ModelResult,
    Observe(..),

    -- Variable projections
    GVar(..),
    Op(..),
    mapGVar,

    -- Command construction
    LockstepCmd(..),
    toLockstepCommand,    -- LockstepCmd → Hedgehog Command

    -- State wrapper
    LockstepState(..),
    initialLockstepState,
    getModelState,

    -- Running tests
    lockstepProperty,     -- sequential
    lockstepParallel,     -- parallel linearizability

    -- Tagging
    lockstepLabelledExamples,

    -- Re-exports
    module Hedgehog,
  ) where
```

### Example: File System Test

```haskell
type instance ModelResult FsState IO.Handle = MHandle
type instance ModelResult FsState (Either Err a) = Either Err (ModelResult FsState a)
-- etc.

data OpenInput v = OpenInput File
  deriving (Show)
instance HTraversable OpenInput where htraverse _ (OpenInput f) = pure (OpenInput f)

data WriteInput v = WriteInput (GVar' Op IO.Handle v) String
  deriving (Show)
instance HTraversable WriteInput where
  htraverse f (WriteInput (GVar' v op) s) =
    (\v' -> WriteInput (GVar' v' op) s) <$> htraverse f v

cmdOpen :: LockstepCmd Gen (ReaderT FilePath IO) FsState
cmdOpen = LockstepCmd {
    lsCmdGen = \st -> Just $ OpenInput <$> genFile st,
    lsCmdExec = \(OpenInput f) -> liftIO $ tryOpen f,
    lsCmdModel = \st (OpenInput f) ->
      let (result, mock') = Mock.mOpen f (mockFs st)
      in (bimap id (\(h,fp) -> (h, fp)) result, st { mockFs = mock' }),
    lsCmdRequire = \_ _ -> True,
    lsCmdTag = \st (OpenInput _) result ->
      [show OpenTwo | Set.size (openedFiles st) >= 2]
  }
```

---

## 5. Improvements Beyond a Direct Port

### 5.1 Type-Level Observability

Instead of runtime `Observable` GADTs, use a type class with a default:

```haskell
class Observe state a where
  type Observed state a :: Type
  type Observed state a = a  -- default: same type, fully observable

  observeReal  :: a -> Observed state a
  default observeReal :: (a ~ Observed state a) => a -> Observed state a
  observeReal = id

  observeModel :: ModelResult state a -> Observed state a

-- For handles: override
instance Observe FsState IO.Handle where
  type Observed FsState IO.Handle = ()  -- unobservable
  observeReal  _ = ()
  observeModel _ = ()

-- For strings: use default
instance Observe FsState String  -- nothing to write!
```

This eliminates most of the boilerplate. Only types with model/real mismatches need instances.

### 5.2 Automatic `usedVars` via `HTraversable`

Hedgehog already requires `HTraversable` for inputs. This serves the same purpose as `usedVars` — it identifies which variables an action references. The library can extract variables automatically from the traversal, eliminating the manual `usedVars` definition.

### 5.3 Better Error Messages

Lockstep currently shows:
```
System under test returned: OEither (Left (OId AlreadyExists))
but model returned:         OEither (Left (OId DoesNotExist))
```

With Hedgehog's diff infrastructure, we can show:
```
━━━ Postcondition failed ━━━
MkDir (Dir ["x"])

  Expected (from model): Left DoesNotExist
  Got (from system):     Left AlreadyExists
                               ^^^^^^^^^^^
```

### 5.4 Compositional Commands

Allow combining lockstep commands from different subsystems:

```haskell
combineCommands :: [LockstepCmd gen m stateA]
                -> [LockstepCmd gen m stateB]
                -> [LockstepCmd gen m (stateA, stateB)]
```

This would allow testing interactions between two subsystems that share a backend.

### 5.5 Property-Level Assertions Beyond Lockstep

The pure lockstep postcondition (model == system) is powerful but sometimes you want *additional* assertions. Allow users to attach extra `Ensure` callbacks:

```haskell
data LockstepCmd gen m state = ... | LockstepCmd {
    ...,
    lsCmdEnsureExtra :: state Concrete -> state Concrete
                     -> input Concrete -> output -> Test ()
  }
```

For example: "after every Write, the file size in the model is positive."

### 5.6 Consider `falsify` Instead of Hedgehog

Edsko himself developed `falsify`, a library with **internal shrinking** that works across monadic bind (unlike Hedgehog's integrated shrinking). If we're rebuilding from scratch, `falsify` might be the better foundation:

- **Internal shrinking** means shrinking works correctly even when later generators depend on earlier results — critical for stateful testing where the generator for step N depends on the model state after step N-1.
- Hedgehog's integrated shrinking breaks down with `>>=` (it can't shrink the left side of a bind independently from the right).
- For stateful testing specifically, this matters when an action's arguments are generated based on the current model state.

However, `falsify` is newer, less battle-tested, and lacks Hedgehog's `executeParallel`. A pragmatic choice would be Hedgehog now (for ecosystem and parallel testing) with an eye toward `falsify` later.

---

## 6. Summary: What Changes, What Stays

| Aspect | quickcheck-lockstep | hedgehog-lockstep |
|--------|-------------------|-------------------|
| **Shrinking** | Manual (`shrinkWithVars`) | Integrated (free) |
| **Variables** | `GVar` with `Op` DSL | `GVar'` adapted for `Symbolic`/`Concrete` |
| **Model values** | `ModelValue` GADT | `ModelResult` type family + `Observe` class |
| **Observability** | `Observable` GADT + `observeModel`/`observeReal` | `Observed` associated type with defaults |
| **Variable tracking** | Manual `usedVars` | Automatic via `HTraversable` |
| **Parallel testing** | Not supported | Supported via `executeParallel` |
| **Tagging** | Requires model re-execution | Can tag during execution |
| **Error messages** | Custom `Show` for `Observable` | Hedgehog's diff infrastructure |
| **Dependency stack** | QC + qc-dynamic + qc-lockstep | Hedgehog + hedgehog-lockstep |
| **Command structure** | Single GADT + typeclass | List of `Command` records |

### What's Lost

- **Dynamic logic**: `quickcheck-dynamic` supports DL specifications; Hedgehog does not.
- **Single GADT elegance**: The single `Action` GADT in lockstep is arguably more elegant than a list of `Command` records. Pattern matching on a closed GADT gives exhaustiveness checking.
- **`labelledExamples`**: QuickCheck has this; Hedgehog doesn't have a direct equivalent (though we can approximate with classification).

### What's Gained

- **No manual shrinking**: The single biggest ergonomic improvement.
- **Parallel testing**: Linearizability checking for free.
- **Less boilerplate**: ~40% less code per test through defaults and `HTraversable`.
- **Better error output**: Hedgehog's pretty-printing and diff support.
- **Simpler mental model**: Two layers instead of three.
