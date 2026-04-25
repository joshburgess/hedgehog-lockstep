# hedgehog-lockstep

Lockstep-style stateful property testing for [Hedgehog](https://hedgehog.qa/).

This library ports the ideas from [quickcheck-lockstep](https://github.com/well-typed/quickcheck-lockstep) onto Hedgehog's `Command`-based state machine framework, giving you:

- **Integrated shrinking** (no manual `shrinkWithVars`)
- **Parallel testing** via Hedgehog's `executeParallel`
- **Less boilerplate** than the quickcheck-lockstep / quickcheck-dynamic / QuickCheck stack
- **GVar projections** for referencing components of previous action outputs

## How it works

Write a pure **model** of your stateful system. Define **commands** that pair a real operation with its model interpretation. The library generates random command sequences, executes them against both the model and the real system, and checks that results agree at every step.

If they disagree, Hedgehog's integrated shrinking finds a minimal counterexample automatically.

## Quick example

Test a mutable key-value store against a pure `Map`:

```haskell
-- Model is a pure Map
type Model = Map String Int

-- Define inputs as barbies-style HKD types
data PutInput v = PutInput !String !Int deriving (Show)
instance FunctorB PutInput where bmap _ (PutInput k v) = PutInput k v
instance TraversableB PutInput where btraverse _ (PutInput k v) = pure (PutInput k v)

data GetInput v = GetInput !String deriving (Show)
instance FunctorB GetInput where bmap _ (GetInput k) = GetInput k
instance TraversableB GetInput where btraverse _ (GetInput k) = pure (GetInput k)

-- Define commands
cmdPut :: IORef (Map String Int) -> LockstepCmd (PropertyT IO) Model
cmdPut store = LockstepCmd
  { lsCmdGen        = \_ -> Just $ PutInput <$> Gen.element ["a","b","c"] <*> Gen.int (Range.linear 0 100)
  , lsCmdExec       = \(PutInput k v) -> evalIO $ modifyIORef' store (Map.insert k v) >> pure ()
  , lsCmdModel      = \st (PutInput k v) -> ((), Map.insert k v (getModel st))
  , lsCmdRequire    = \_ _ -> True
  , lsCmdObserve    = \() () -> pure ()
  , lsCmdInvariants = \_ _ -> pure ()
  , lsCmdTag        = \_ _ _ -> []
  }

cmdGet :: IORef (Map String Int) -> LockstepCmd (PropertyT IO) Model
cmdGet store = LockstepCmd
  { lsCmdGen        = \_ -> Just $ GetInput <$> Gen.element ["a","b","c"]
  , lsCmdExec       = \(GetInput k) -> evalIO $ Map.lookup k <$> readIORef store
  , lsCmdModel      = \st (GetInput k) -> (Map.lookup k (getModel st), getModel st)
  , lsCmdRequire    = \_ _ -> True
  , lsCmdObserve    = \expected actual -> expected === actual
  , lsCmdInvariants = \_ _ -> pure ()
  , lsCmdTag        = \_ _ _ -> []
  }

-- Run the property
prop_kvStore :: Property
prop_kvStore =
  lockstepPropertyWith
    Map.empty           -- initial model
    50                  -- max actions per test
    (newIORef Map.empty)           -- create resource
    (\ref -> writeIORef ref Map.empty)  -- reset before execution
    (\ref -> [cmdPut ref, cmdGet ref])  -- commands
```

## GVar: referencing previous outputs

When a command returns a compound type (e.g., `Either String (Handle, FilePath)`), later commands may need to reference a component of that result. `GVar` with `Op` projections handles this:

```haskell
-- Open returns Either String (Handle, Name)
-- Write needs the Handle, projected via OpRight >>> OpFst

data WriteInput v = WriteInput !(GVar Handle v) !String deriving (Show)

instance FunctorB WriteInput where
  bmap f (WriteInput gv s) = WriteInput (bmap f gv) s
instance TraversableB WriteInput where
  btraverse f (WriteInput gv s) = (\gv' -> WriteInput gv' s) <$> btraverse f gv

cmdWrite ref = LockstepCmd
  { lsCmdGen = \st ->
      case varsOfType @OpenResult st of
        [] -> Nothing
        vars -> Just $ do
          var <- Gen.element vars
          let op :: Op OpenResult Handle
              op = OpRight >>> OpFst
          content <- Gen.string (Range.linear 0 20) Gen.alpha
          pure $ WriteInput (mkGVar var op) content

  , lsCmdExec = \(WriteInput gv content) ->
      case concreteGVar gv of
        Just h  -> evalIO $ write ref h content
        Nothing -> pure (Left "bad handle")

  , lsCmdModel = \st (WriteInput gv content) ->
      case resolveGVar gv (getEntries st) of
        Just h  -> modelWrite (getModel st) h content
        Nothing -> (Left "bad handle", getModel st)
  , ...
  }
```

## Available Op projections

| Op | Projects from | To |
|----|--------------|-----|
| `OpId` | `a` | `a` |
| `OpFst` | `(a, b)` | `a` |
| `OpSnd` | `(a, b)` | `b` |
| `OpLeft` | `Either a b` | `a` (partial) |
| `OpRight` | `Either a b` | `b` (partial) |
| `OpComp f g` | composition | `g` then `f` |
| `f >>> g` | left-to-right | `f` then `g` |

Partial projections (`OpLeft`, `OpRight`) return `Nothing` on mismatch.

If you already hold a `GVar` and want to project further, use `mapGVar`:

```haskell
gv     :: GVar (Either err (Handle, Name)) v
gvHandle = mapGVar (OpRight >>> OpFst) gv  -- :: GVar Handle v
```

### Custom projections

The built-in `Op` covers pair, sum, identity, and composition. If you need
a projection it doesn't have (list head, record fields, custom decoders),
define your own GADT and write an `InterpretOp` instance plus a `Show`
instance. `mkGVar` and `mapGVar` accept any op that satisfies those
constraints:

```haskell
data MyOp a b where
  MyOfOp   :: Op a b -> MyOp a b   -- embed the built-ins
  MyOpHead :: MyOp [a] a           -- new: list head

instance InterpretOp MyOp where
  interpretOp (MyOfOp op) x       = interpretOp op x
  interpretOp MyOpHead    []      = Nothing
  interpretOp MyOpHead    (x : _) = Just x

instance Show (MyOp a b) where
  show (MyOfOp op) = show op
  show MyOpHead    = "head"

-- now usable anywhere a built-in Op was:
gvHead :: Var [Int] v -> GVar Int v
gvHead var = mkGVar var (MyOpHead :: MyOp [Int] Int)
```

The library uses the `InterpretOp` instance to apply the projection during
both model and real resolution; the `Show` instance feeds the `gvarLabel`
chain shown in failure messages. See `test/Test/CustomOp.hs` for a full
end-to-end example.

## Structured observations

Plain `lsCmdObserve = \m r -> m === r` only works when the model output and the real output have the same type. Real APIs often violate that: a real `open` returns a `Handle` while the model returns an index; the real call returns a verbose error string while the model returns an abstract tag; both sides return rich records but only a subset of fields should be compared.

`Observation modelOutput output` is a small typed DSL that captures the common patterns:

```haskell
data Observation modelOutput output where
  ObserveEq      :: (Eq output, Show output) => Observation output output
  ObserveProject :: (Eq obs, Show obs)
                 => (modelOutput -> obs)
                 -> (output -> obs)
                 -> Observation modelOutput output
  ObservePair    :: Observation m1 o1
                 -> Observation m2 o2
                 -> Observation (m1, m2) (o1, o2)
  ObserveCustom  :: (modelOutput -> output -> Test ())
                 -> Observation modelOutput output

runObservation :: Observation modelOutput output -> modelOutput -> output -> Test ()
```

`runObservation` produces a function of exactly the shape `lsCmdObserve` expects, so:

```haskell
-- Same type on both sides:
lsCmdObserve = runObservation ObserveEq

-- Project both sides through a normaliser:
lsCmdObserve = runObservation (ObserveProject normaliseModel normaliseReal)

-- Tuple of independent observations:
lsCmdObserve = runObservation (ObservePair ObserveEq (ObserveProject id parseCount))

-- Escape hatch (equivalent to writing it inline):
lsCmdObserve = runObservation (ObserveCustom $ \m r -> ...)
```

This is the analogue of `quickcheck-lockstep`'s `Observable`/`ModelValue` GADT split, but lighter: you can mix structured observations with `lsCmdObserve = \m r -> ...` freely, file by file. See `test/Test/Observation.hs` for an example using `ObservePair` + `ObserveProject`.

## LockstepCmd fields

| Field | Type | Purpose |
|-------|------|---------|
| `lsCmdGen` | `LockstepState model Symbolic -> Maybe (Gen (input Symbolic))` | Generate an input, or `Nothing` if inapplicable |
| `lsCmdExec` | `input Concrete -> m output` | Execute against the real system |
| `lsCmdModel` | `forall v. Ord1 v => LockstepState model v -> input v -> (modelOutput, model)` | Pure model step |
| `lsCmdRequire` | `model -> input Symbolic -> Bool` | Additional preconditions |
| `lsCmdObserve` | `modelOutput -> output -> Test ()` | Compare model and real results |
| `lsCmdInvariants` | `model -> output -> Test ()` | System invariants beyond the lockstep check |
| `lsCmdTag` | `model -> model -> modelOutput -> [String]` | Per-step coverage tags (pre-state, post-state, model output) |

## Running tests

```haskell
-- Sequential (most common)
lockstepProperty     :: model -> Int -> [LockstepCmd (PropertyT IO) model] -> Property
lockstepPropertyWith :: model -> Int -> IO env -> (env -> IO ()) -> (env -> [LockstepCmd ...]) -> Property

-- Parallel (linearizability)
lockstepParallel     :: model -> Int -> Int -> [LockstepCmd (PropertyT IO) model] -> Property
lockstepParallelWith :: model -> Int -> Int -> IO env -> (env -> IO ()) -> (env -> [LockstepCmd ...]) -> Property

-- Manual (use Gen.sequential / executeSequential directly)
lockstepCommands :: [LockstepCmd m model] -> [Command Gen m (LockstepState model)]
```

Use the `*With` variants when commands need IO resources (e.g., `IORef`, database connections). The reset callback runs before each execution attempt, including during shrinking.

The bare `lockstepProperty` and `lockstepParallel` are for commands that don't need a resource. The commands still run in `PropertyT IO` (Hedgehog requires it), but `lsCmdExec` can simply lift a pure value with `pure`. See `test/Test/PureSort.hs` for a fully pure example.

Parallel operations must be thread-safe: use `atomicModifyIORef'`, `MVar`, or `TVar` rather than `modifyIORef'`.

### Running tests in your own monad

If your commands are written in a custom monad stack (e.g., `ReaderT Connection (PropertyT IO)`), use the `*M` variants to plug in a runner. They take a natural transformation that lowers your monad back to `PropertyT IO`:

```haskell
-- Sequential, in an arbitrary monad m
lockstepPropertyM
  :: (Show model, Monad m)
  => model -> Int
  -> (forall a. m a -> PropertyT IO a)  -- ^ how to run m
  -> [LockstepCmd m model]
  -> Property

-- With an IO resource: the runner receives the resource per test case
lockstepPropertyWithM
  :: (Show model, Monad m)
  => model -> Int
  -> IO env -> (env -> IO ())
  -> (forall a. env -> m a -> PropertyT IO a)
  -> (env -> [LockstepCmd m model])
  -> Property

-- Parallel variants: lockstepParallelM, lockstepParallelWithM
```

For a `ReaderT env (PropertyT IO)` stack the runner is just `flip runReaderT`:

```haskell
prop_kv :: Property
prop_kv =
  lockstepPropertyWithM
    Map.empty 50
    newStore resetStore
    (\store m -> runReaderT m store)
    (\_store -> [cmdPut, cmdGet])
```

Now the command bodies can `ask` the resource from the environment instead of closing over it. See `test/Test/ReaderKV.hs` for the full example.

## Coverage labels

There are two ways to get per-test coverage. Use whichever is more natural for the tag in question.

**`lsCmdTag`** is the dedicated per-step tagging hook (the analogue of `quickcheck-lockstep`'s `tagStep`). It receives the pre-step model, the post-step model, and the model's predicted output, and returns a list of string tags. Each tag is fed to `Hedgehog.label`. This is the right place for tags that depend on how the model state changed during the step.

```haskell
cmdPut store = LockstepCmd
  { ...
  , lsCmdTag = \pre post () ->
      ["Put", if Map.size post > Map.size pre then "Put new key" else "Put overwrite"]
  }
```

**`lsCmdObserve`** runs in `Test ()`, so Hedgehog's `label`, `classify`, and `cover` work directly inside it. Use this for tags that depend on the real output (which `lsCmdTag` doesn't see).

```haskell
cmdGet store = LockstepCmd
  { ...
  , lsCmdObserve = \expected actual -> do
      label "Get"
      classify "Get hit"  (isJust expected)
      classify "Get miss" (isNothing expected)
      expected === actual
  }
```

Hedgehog aggregates the labels across the run and prints a per-tag distribution table in the test summary. See `test/Test/KVStore.hs` for both styles.

### Labelled examples (model-only)

Hedgehog's distribution table tells you how often each tag fires across a real test run, but it doesn't show *what kind of trace* produced each tag. `lockstepLabelledExamples` does, by running the model side only (no IO) and printing one shortest sampled trace per tag:

```haskell
import Hedgehog.Lockstep

main :: IO ()
main = do
  _ <- lockstepLabelledExamples
         100         -- trials
         12          -- max actions per trial
         Map.empty   -- initial model
         [cmdPut, cmdDelete]
  pure ()
```

Sample output:

```
lockstepLabelledExamples: 4 tag(s) observed.
Tag "Put new key" (1 action(s)):
  PutInput "b" 18  [Put, Put new key]

Tag "Put overwrite" (2 action(s)):
  PutInput "a" 0   [Put, Put new key]
  PutInput "a" 12  [Put, Put overwrite]

...
```

This is the analogue of QuickCheck's `labelledExamples` and `quickcheck-lockstep`'s `tagActions`. Use it to confirm your generator and tagging actually exercise the cases you care about, before paying for a full property run. Because nothing real executes, `lsCmdExec` can be `error "unused"` if you want to write a coverage-only check.

## Failure diagnostics

When a test fails, `hedgehog-lockstep` automatically `footnote`s the post-step model state at every command. Hedgehog only displays footnotes on failure, so passing tests pay nothing, but a failing test now shows the model trail alongside the shrunken command sequence (the analogue of `quickcheck-lockstep`'s `monitoring` counterexample enrichment). This requires `Show model`, which is already needed by Hedgehog's `Gen.sequential`.

## System invariants

`lsCmdObserve` is for the lockstep equality check between the model output and the real output. For invariants on the post-state model itself (or on the real output independent of the model), use `lsCmdInvariants`. It receives the post-state model and the real output, runs in `Test ()`, and fires after every command.

```haskell
cmdSize store = LockstepCmd
  { ...
  , lsCmdObserve    = \expected actual -> expected === actual
  , lsCmdInvariants = \_ size -> assert (size >= 0)
  }
```

If you have nothing extra to check, write `lsCmdInvariants = \_ _ -> pure ()`.

## Building

Requires GHC 9.10.3:

```
cabal build
cabal test
```

## Status

This is a v0.1 implementation. See the [design document](docs/DESIGN.md) for the full rationale and comparison with quickcheck-lockstep.

### Known caveats

- **`Ord output` constraint**. `LockstepCmd` requires `Ord output` on command output types. This is needed for Var-identity-based model environment lookup. Most types satisfy this; truly opaque types (e.g., `IO.Handle`) would need a newtype wrapper that defines ordering (for example, by an allocation counter).

- **Depends on `Hedgehog.Internal.State`**. The library imports a handful of types and constructors (`Var`, `Symbolic`, `Concrete`, `Command`, `Callback`) from Hedgehog's internal module because the public surface doesn't expose enough to drive the state machine framework directly. Hedgehog's PVP guarantees don't cover internal modules, so a Hedgehog minor release could in principle break the build. The cabal file pins `hedgehog >=1.4 && <1.6` to limit exposure; bump the upper bound only after rebuilding against the new version.
