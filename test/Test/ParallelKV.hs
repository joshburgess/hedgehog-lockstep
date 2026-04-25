{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Test.ParallelKV
  ( prop_kvParallel
  , prop_kvParallelBuggy
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog.Lockstep

-- ---------------------------------------------------------------------------
-- Thread-safe KV store using atomicModifyIORef'
-- ---------------------------------------------------------------------------

type Store = IORef (Map String Int)

newStore :: IO Store
newStore = newIORef Map.empty

resetStore :: Store -> IO ()
resetStore ref = writeIORef ref Map.empty

-- | Atomic put. Returns the size of the store after the insert.
atomicPut :: Store -> String -> Int -> IO Int
atomicPut ref k v =
  atomicModifyIORef' ref $ \m ->
    let m' = Map.insert k v m in (m', Map.size m')

-- | Atomic get.
atomicGet :: Store -> String -> IO (Maybe Int)
atomicGet ref k = Map.lookup k <$> readIORef ref

-- | Deliberately *broken* put: reads, then writes, with no atomicity.
-- Used to verify that 'lockstepParallel' detects non-linearizable behavior.
racyPut :: Store -> String -> Int -> IO Int
racyPut ref k v = do
  m <- readIORef ref
  let m' = Map.insert k v m
  writeIORef ref m'
  pure (Map.size m')

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

type Model = Map String Int

-- ---------------------------------------------------------------------------
-- Inputs
-- ---------------------------------------------------------------------------

data PutInput v = PutInput !String !Int
  deriving stock (Show)

instance FunctorB PutInput where bmap _ (PutInput k v) = PutInput k v
instance TraversableB PutInput where btraverse _ (PutInput k v) = pure (PutInput k v)

data GetInput v = GetInput !String
  deriving stock (Show)

instance FunctorB GetInput where bmap _ (GetInput k) = GetInput k
instance TraversableB GetInput where btraverse _ (GetInput k) = pure (GetInput k)

-- ---------------------------------------------------------------------------
-- Commands
-- ---------------------------------------------------------------------------

genKey :: Gen String
genKey = Gen.element ["a", "b", "c"]

genVal :: Gen Int
genVal = Gen.int (Range.linear 0 100)

cmdPut :: (Store -> String -> Int -> IO Int) -> Store -> LockstepCmd (PropertyT IO) Model
cmdPut putImpl store = LockstepCmd
  { lsCmdGen = \_ -> Just $ PutInput <$> genKey <*> genVal

  , lsCmdExec = \(PutInput k v) -> evalIO $ putImpl store k v

  , lsCmdModel = \st (PutInput k v) ->
      let m = Map.insert k v (getModel st)
      in (Map.size m, m)

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \expected actual -> expected === actual

  , lsCmdInvariants = \_ _ -> pure ()
  }

cmdGet :: Store -> LockstepCmd (PropertyT IO) Model
cmdGet store = LockstepCmd
  { lsCmdGen = \_ -> Just $ GetInput <$> genKey

  , lsCmdExec = \(GetInput k) -> evalIO $ atomicGet store k

  , lsCmdModel = \st (GetInput k) ->
      let m = getModel st
      in (Map.lookup k m, m)

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \expected actual -> expected === actual

  , lsCmdInvariants = \_ _ -> pure ()
  }

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | Parallel test with atomic operations: must pass (all executions are linearizable).
prop_kvParallel :: Property
prop_kvParallel =
  lockstepParallelWith
    Map.empty
    10  -- sequential prefix
    5   -- parallel branch length
    newStore
    resetStore
    (\ref -> [cmdPut atomicPut ref, cmdGet ref])

-- | Parallel test with a deliberately racy put: used to verify that the
-- library actually exercises concurrent execution. Not used in main tests
-- because race detection is inherently flaky; kept here for manual exploration.
prop_kvParallelBuggy :: Property
prop_kvParallelBuggy =
  lockstepParallelWith
    Map.empty
    5
    5
    newStore
    resetStore
    (\ref -> [cmdPut racyPut ref, cmdGet ref])
