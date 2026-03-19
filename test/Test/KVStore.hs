{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Test.KVStore
  ( prop_kvSequential
  ) where

import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog.Lockstep

-- ---------------------------------------------------------------------------
-- System under test: a mutable key-value store backed by IORef
-- ---------------------------------------------------------------------------

type Store = IORef (Map String Int)

newStore :: IO Store
newStore = newIORef Map.empty

putKV :: Store -> String -> Int -> IO ()
putKV ref k v = modifyIORef' ref (Map.insert k v)

getKV :: Store -> String -> IO (Maybe Int)
getKV ref k = Map.lookup k <$> readIORef ref

deleteKV :: Store -> String -> IO ()
deleteKV ref k = modifyIORef' ref (Map.delete k)

sizeKV :: Store -> IO Int
sizeKV ref = Map.size <$> readIORef ref

-- ---------------------------------------------------------------------------
-- Model: pure Map
-- ---------------------------------------------------------------------------

type Model = Map String Int

-- ---------------------------------------------------------------------------
-- Inputs (barbies-style HKD, parameterized by v)
-- ---------------------------------------------------------------------------

data PutInput v = PutInput !String !Int
  deriving stock (Show)

instance FunctorB PutInput where bmap _ (PutInput k v) = PutInput k v
instance TraversableB PutInput where btraverse _ (PutInput k v) = pure (PutInput k v)

data GetInput v = GetInput !String
  deriving stock (Show)

instance FunctorB GetInput where bmap _ (GetInput k) = GetInput k
instance TraversableB GetInput where btraverse _ (GetInput k) = pure (GetInput k)

data DeleteInput v = DeleteInput !String
  deriving stock (Show)

instance FunctorB DeleteInput where bmap _ (DeleteInput k) = DeleteInput k
instance TraversableB DeleteInput where btraverse _ (DeleteInput k) = pure (DeleteInput k)

data SizeInput v = SizeInput
  deriving stock (Show)

instance FunctorB SizeInput where bmap _ SizeInput = SizeInput
instance TraversableB SizeInput where btraverse _ SizeInput = pure SizeInput

-- ---------------------------------------------------------------------------
-- Commands
-- ---------------------------------------------------------------------------

genKey :: Gen String
genKey = Gen.element ["a", "b", "c", "d"]

genVal :: Gen Int
genVal = Gen.int (Range.linear 0 100)

cmdPut :: Store -> LockstepCmd (PropertyT IO) Model
cmdPut store = LockstepCmd
  { lsCmdGen = \_ -> Just $ PutInput <$> genKey <*> genVal

  , lsCmdExec = \(PutInput k v) -> evalIO $ putKV store k v

  , lsCmdModel = \st (PutInput k v) ->
      let model = getModel st
      in ((), Map.insert k v model)

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \() () -> pure ()
  }

cmdGet :: Store -> LockstepCmd (PropertyT IO) Model
cmdGet store = LockstepCmd
  { lsCmdGen = \_ -> Just $ GetInput <$> genKey

  , lsCmdExec = \(GetInput k) -> evalIO $ getKV store k

  , lsCmdModel = \st (GetInput k) ->
      let model = getModel st
      in (Map.lookup k model, model)

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \expected actual -> expected === actual
  }

cmdDelete :: Store -> LockstepCmd (PropertyT IO) Model
cmdDelete store = LockstepCmd
  { lsCmdGen = \st ->
      let model = getModel st
      in if Map.null model
         then Nothing
         else Just $ DeleteInput <$> Gen.element (Map.keys model)

  , lsCmdExec = \(DeleteInput k) -> evalIO $ deleteKV store k

  , lsCmdModel = \st (DeleteInput k) ->
      let model = getModel st
      in ((), Map.delete k model)

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \() () -> pure ()
  }

cmdSize :: Store -> LockstepCmd (PropertyT IO) Model
cmdSize store = LockstepCmd
  { lsCmdGen = \_ -> Just $ pure SizeInput

  , lsCmdExec = \SizeInput -> evalIO $ sizeKV store

  , lsCmdModel = \st SizeInput ->
      let model = getModel st
      in (Map.size model, model)

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \expected actual -> expected === actual
  }

-- ---------------------------------------------------------------------------
-- Property
-- ---------------------------------------------------------------------------

prop_kvSequential :: Property
prop_kvSequential = property $ do
  store <- evalIO newStore
  let cmds = lockstepCommands
        [ cmdPut store
        , cmdGet store
        , cmdDelete store
        , cmdSize store
        ]
      initial = initialLockstepState Map.empty
  actions <- forAll $
    Gen.sequential (Range.linear 1 50) initial cmds
  executeSequential initial actions
