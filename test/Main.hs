module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.KVStore (prop_kvSequential)
import Test.HandleStore (prop_handleSequential)
import Test.BuggyCounter (prop_buggyCounterDetected)
import Test.OpProjections (prop_opProjections)
import Test.ParallelKV (prop_kvParallel)
import Test.PureSort (prop_pureSort)
import Test.ReaderKV (prop_readerKV)
import Test.UnitCoverage (prop_applyOp, prop_gvarLabel, prop_mapGVar, prop_mkGVarId)
import Hedgehog (check)

main :: IO ()
main = do
  ok1 <- check prop_kvSequential
  ok2 <- check prop_handleSequential
  ok3 <- check prop_kvParallel
  ok4 <- check prop_pureSort
  ok5 <- check prop_opProjections
  ok6 <- check prop_applyOp
  ok7 <- check prop_gvarLabel
  ok8 <- check prop_mkGVarId
  ok9 <- check prop_mapGVar
  ok10 <- check prop_readerKV
  -- BuggyCounter should FAIL (model is deliberately wrong).
  -- We verify the failure is detected.
  ok11 <- check prop_buggyCounterDetected
  let bugDetected = not ok11
  putStrLn $ if bugDetected
    then "  Buggy model correctly detected"
    else "  Buggy model was NOT detected (bug in lockstep!)"
  if and [ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8, ok9, ok10, bugDetected]
    then exitSuccess
    else exitFailure
