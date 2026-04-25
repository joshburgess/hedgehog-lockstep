{-# LANGUAGE RankNTypes #-}
module Test.ReaderKV
  ( prop_readerKV
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog.Lockstep

-- ---------------------------------------------------------------------------
-- Exercises 'lockstepPropertyWithM': commands written in
-- @ReaderT Store (PropertyT IO)@ rather than @PropertyT IO@ directly.
-- The runner ('runReaderT') is supplied separately, so command bodies
-- read the store from the environment instead of closing over it.
-- ---------------------------------------------------------------------------

type Store = IORef (Map String Int)

type AppM = ReaderT Store (PropertyT IO)

type Model = Map String Int

newStore :: IO Store
newStore = newIORef Map.empty

resetStore :: Store -> IO ()
resetStore ref = writeIORef ref Map.empty

-- Inputs

data PutInput v = PutInput !String !Int
  deriving stock (Show)

instance FunctorB PutInput where bmap _ (PutInput k v) = PutInput k v
instance TraversableB PutInput where btraverse _ (PutInput k v) = pure (PutInput k v)

data GetInput v = GetInput !String
  deriving stock (Show)

instance FunctorB GetInput where bmap _ (GetInput k) = GetInput k
instance TraversableB GetInput where btraverse _ (GetInput k) = pure (GetInput k)

genKey :: Gen String
genKey = Gen.element ["a", "b", "c"]

genVal :: Gen Int
genVal = Gen.int (Range.linear 0 100)

-- Commands run in 'AppM'. Note: the exec functions never see a 'Store'
-- argument; they read it from the ReaderT environment. The model
-- functions are unchanged, since the model is pure.
cmdPut :: LockstepCmd AppM Model
cmdPut = LockstepCmd
  { lsCmdGen = \_ -> Just $ PutInput <$> genKey <*> genVal

  , lsCmdExec = \(PutInput k v) -> do
      ref <- ask
      liftIO $ modifyIORef' ref (Map.insert k v)

  , lsCmdModel = \st (PutInput k v) ->
      ((), Map.insert k v (getModel st))

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \() () -> pure ()

  , lsCmdInvariants = \_ _ -> pure ()

  , lsCmdTag = \_ _ _ -> []
  }

cmdGet :: LockstepCmd AppM Model
cmdGet = LockstepCmd
  { lsCmdGen = \_ -> Just $ GetInput <$> genKey

  , lsCmdExec = \(GetInput k) -> do
      ref <- ask
      liftIO $ Map.lookup k <$> readIORef ref

  , lsCmdModel = \st (GetInput k) ->
      let model = getModel st
      in (Map.lookup k model, model)

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \expected actual -> expected === actual

  , lsCmdInvariants = \_ _ -> pure ()

  , lsCmdTag = \_ _ _ -> []
  }

prop_readerKV :: Property
prop_readerKV =
  lockstepPropertyWithM
    Map.empty
    50
    newStore
    resetStore
    (\store m -> runReaderT m store)
    (\_store -> [cmdPut, cmdGet])
