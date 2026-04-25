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
  }

cmdGet :: IORef (Map String Int) -> LockstepCmd (PropertyT IO) Model
cmdGet store = LockstepCmd
  { lsCmdGen        = \_ -> Just $ GetInput <$> Gen.element ["a","b","c"]
  , lsCmdExec       = \(GetInput k) -> evalIO $ Map.lookup k <$> readIORef store
  , lsCmdModel      = \st (GetInput k) -> (Map.lookup k (getModel st), getModel st)
  , lsCmdRequire    = \_ _ -> True
  , lsCmdObserve    = \expected actual -> expected === actual
  , lsCmdInvariants = \_ _ -> pure ()
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

## LockstepCmd fields

| Field | Type | Purpose |
|-------|------|---------|
| `lsCmdGen` | `LockstepState model Symbolic -> Maybe (Gen (input Symbolic))` | Generate an input, or `Nothing` if inapplicable |
| `lsCmdExec` | `input Concrete -> m output` | Execute against the real system |
| `lsCmdModel` | `forall v. Ord1 v => LockstepState model v -> input v -> (modelOutput, model)` | Pure model step |
| `lsCmdRequire` | `model -> input Symbolic -> Bool` | Additional preconditions |
| `lsCmdObserve` | `modelOutput -> output -> Test ()` | Compare model and real results |
| `lsCmdInvariants` | `model -> output -> Test ()` | System invariants beyond the lockstep check |

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

Parallel operations must be thread-safe: use `atomicModifyIORef'`, `MVar`, or `TVar` rather than `modifyIORef'`.

## Coverage labels

`lsCmdObserve` runs in `Test ()`, so Hedgehog's `label`, `classify`, and `cover` work directly inside it. There's no separate tagging API.

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

Hedgehog aggregates the labels across the run and prints them in the failure report (or always, if you use `cover`). See `test/Test/KVStore.hs` for a full example.

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

Known limitation: `LockstepCmd` requires `Ord output` on command output types. This is needed for Var-identity-based model environment lookup. Most types satisfy this; truly opaque types (e.g., `IO.Handle`) would need a newtype wrapper.
