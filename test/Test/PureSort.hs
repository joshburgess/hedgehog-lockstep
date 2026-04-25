{-# LANGUAGE RankNTypes #-}
module Test.PureSort
  ( prop_pureSort
  ) where

import Data.List qualified as List
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog.Lockstep

-- ---------------------------------------------------------------------------
-- Exercises 'lockstepProperty' (the no-resource variant).
--
-- Each command is independent and takes no IO state: the "real" system is
-- 'Data.List.sort' and the model is an insertion sort. Both should agree
-- on every input list.
-- ---------------------------------------------------------------------------

type Model = ()

data SortInput v = SortInput ![Int]
  deriving stock (Show)

instance FunctorB SortInput where
  bmap _ (SortInput xs) = SortInput xs

instance TraversableB SortInput where
  btraverse _ (SortInput xs) = pure (SortInput xs)

insertionSort :: [Int] -> [Int]
insertionSort = foldr insert []
  where
    insert x [] = [x]
    insert x (y : ys)
      | x <= y    = x : y : ys
      | otherwise = y : insert x ys

cmdSort :: LockstepCmd (PropertyT IO) Model
cmdSort = LockstepCmd
  { lsCmdGen = \_ ->
      Just $ SortInput <$>
        Gen.list (Range.linear 0 20) (Gen.int (Range.linear (-50) 50))

  , lsCmdExec = \(SortInput xs) -> pure (List.sort xs)

  , lsCmdModel = \_ (SortInput xs) -> (insertionSort xs, ())

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \expected actual -> expected === actual

  -- System invariant: a sorted list is monotonically non-decreasing.
  , lsCmdInvariants = \_ xs ->
      assert (and (zipWith (<=) xs (drop 1 xs)))

  , lsCmdTag = \_ _ _ -> []
  }

prop_pureSort :: Property
prop_pureSort = lockstepProperty () 20 [cmdSort]
