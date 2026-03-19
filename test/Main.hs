module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.KVStore (prop_kvSequential)
import Test.HandleStore (prop_handleSequential)
import Test.BuggyCounter (prop_buggyCounterDetected)
import Hedgehog (check)

main :: IO ()
main = do
  ok1 <- check prop_kvSequential
  ok2 <- check prop_handleSequential
  -- BuggyCounter should FAIL (model is deliberately wrong).
  -- We verify the failure is detected.
  ok3 <- check prop_buggyCounterDetected
  let bugDetected = not ok3
  putStrLn $ if bugDetected
    then "  ✓ Buggy model correctly detected"
    else "  ✗ Buggy model was NOT detected (bug in lockstep!)"
  if ok1 && ok2 && bugDetected then exitSuccess else exitFailure
