{-# LANGUAGE RankNTypes #-}
module Test.LabelledExamples
  ( prop_labelledExamples
  ) where

import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog.Lockstep

-- ---------------------------------------------------------------------------
-- Exercises 'lockstepLabelledExamples'.
--
-- 'lockstepLabelledExamples' runs the model only (no IO), so the unused
-- 'IORef' resource here is just a placeholder so the command record
-- compiles. A property test below asserts that every tag we expect the
-- generator to hit actually appears in the collected examples.
-- ---------------------------------------------------------------------------

type Store = IORef (Map String Int)
type Model = Map String Int

data PutInput v = PutInput !String !Int
  deriving stock (Show)
instance FunctorB PutInput where bmap _ (PutInput k v) = PutInput k v
instance TraversableB PutInput where btraverse _ (PutInput k v) = pure (PutInput k v)

data DeleteInput v = DeleteInput !String
  deriving stock (Show)
instance FunctorB DeleteInput where bmap _ (DeleteInput k) = DeleteInput k
instance TraversableB DeleteInput where btraverse _ (DeleteInput k) = pure (DeleteInput k)

genKey :: Gen String
genKey = Gen.element ["a", "b"]

cmdPut :: Store -> LockstepCmd (PropertyT IO) Model
cmdPut _ = LockstepCmd
  { lsCmdGen = \_ -> Just $ PutInput <$> genKey <*> Gen.int (Range.linear 0 100)
  , lsCmdExec = \_ -> error "lsCmdExec should not run during labelledExamples"
  , lsCmdModel = \st (PutInput k v) -> ((), Map.insert k v (getModel st))
  , lsCmdRequire = \_ _ -> True
  , lsCmdObserve = \() () -> pure ()
  , lsCmdInvariants = \_ _ -> pure ()
  , lsCmdTag = \pre post () ->
      ["Put", if Map.size post > Map.size pre then "Put new key" else "Put overwrite"]
  }

cmdDelete :: Store -> LockstepCmd (PropertyT IO) Model
cmdDelete _ = LockstepCmd
  { lsCmdGen = \st ->
      let m = getModel st
      in if Map.null m
         then Nothing
         else Just $ DeleteInput <$> Gen.element (Map.keys m)
  , lsCmdExec = \_ -> error "lsCmdExec should not run during labelledExamples"
  , lsCmdModel = \st (DeleteInput k) -> ((), Map.delete k (getModel st))
  , lsCmdRequire = \_ _ -> True
  , lsCmdObserve = \() () -> pure ()
  , lsCmdInvariants = \_ _ -> pure ()
  , lsCmdTag = \_ _ () -> ["Delete"]
  }

-- The "store" is never actually used: we only run the model side.
dummyStore :: Store
dummyStore = error "dummyStore: should not be evaluated by lockstepLabelledExamples"

-- | Expectation: with 100 trials of up to 12 actions each, the generator
-- should certainly hit Put, Put new key, Put overwrite, and Delete.
prop_labelledExamples :: Property
prop_labelledExamples = withTests 1 . property $ do
  examples <- evalIO $
    lockstepLabelledExamples 100 12 Map.empty
      [ cmdPut dummyStore
      , cmdDelete dummyStore
      ]
  let observed = Set.fromList (Map.keys examples)
      required = Set.fromList ["Put", "Put new key", "Put overwrite", "Delete"]
      missing  = Set.difference required observed
  if Set.null missing
    then pure ()
    else do
      footnote $ "Missing tags: " <> show (Set.toList missing)
      failure
