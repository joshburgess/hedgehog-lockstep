{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Model-only driver for collecting one shortest example per tag.
--
-- 'lockstepLabelledExamples' generates random command sequences,
-- /runs only the model side/, accumulates the tags emitted by
-- 'Hedgehog.Lockstep.Command.lsCmdTag', and prints one shortest
-- trace per tag observed. This is the analogue of
-- @QuickCheck@'s @labelledExamples@ and
-- @quickcheck-lockstep@'s @tagActions@: it answers \"is the test
-- generator actually exercising the labelled cases I care about?\"
-- without paying the cost of executing the real system.
--
-- The function never fails. It always prints something. It returns
-- the collected examples as a 'Data.Map.Strict.Map' so callers can
-- post-process them programmatically as well.
module Hedgehog.Lockstep.Examples
  ( lockstepLabelledExamples
  , LabelledExamples
  , ModelStep (..)
  ) where

import Control.Monad (forM_)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Hedgehog.Internal.State (Name (..), Symbolic (..), Var (..))
import Hedgehog.Internal.State qualified

import Hedgehog.Lockstep.Command (LockstepCmd (..))
import Hedgehog.Lockstep.State
  ( LockstepState (..)
  , initialLockstepState
  , insertModelResult
  )

-- | One step in a sampled trace: the rendered (symbolic) input and the
-- list of tags it produced via 'Hedgehog.Lockstep.Command.lsCmdTag'.
data ModelStep = ModelStep
  { stepRendered :: !String
    -- ^ The input rendered via @show@ (the @Show (input Symbolic)@
    -- instance required by 'Hedgehog.Lockstep.Command.LockstepCmd').
  , stepTags     :: ![String]
    -- ^ Tags emitted by 'Hedgehog.Lockstep.Command.lsCmdTag' for this step.
  }
  deriving stock (Show)

-- | A map from tag to the shortest sampled trace that produced it.
--
-- The key is the tag string. The value is the trace, ordered earliest
-- step first, and \"shortest\" is measured by total step count: ties
-- are broken by first-seen.
type LabelledExamples = Map String [ModelStep]

-- | Sample @nTrials@ random command sequences (model-only) of up to
-- @maxActions@ steps each. Collect every tag emitted by
-- 'Hedgehog.Lockstep.Command.lsCmdTag' along the way, keeping the
-- shortest trace seen for each tag. Print the collected examples and
-- return them.
--
-- This runs entirely in the model: 'Hedgehog.Lockstep.Command.lsCmdExec'
-- is /not/ called, so no IO resources or real systems are involved.
-- Use it as a sanity check that your generator and tagging cover the
-- cases you expect, before spending CPU on a real test run.
--
-- The output format is one block per tag:
--
-- @
-- Tag \"Put new key\" (3 actions):
--   PutInput \"a\" 5      [Put, Put new key]
--   GetInput \"b\"        [Get]
--   DeleteInput \"a\"     [Delete]
-- @
--
-- Tags listed on a step are exactly those returned by
-- 'Hedgehog.Lockstep.Command.lsCmdTag' for that step.
lockstepLabelledExamples
  :: forall m model.
     Int
     -- ^ Number of random trials (e.g., @1000@)
  -> Int
     -- ^ Max actions per trial (e.g., @20@)
  -> model
     -- ^ Initial model
  -> [LockstepCmd m model]
     -- ^ Commands (the @m@ parameter is ignored: model-only run)
  -> IO LabelledExamples
lockstepLabelledExamples nTrials maxActions model0 cmds = do
  ref <- newIORef Map.empty
  let trial = do
        steps <- Gen.sample (sampleTrace maxActions model0 cmds)
        recordTrace ref steps
  -- replicateM_ would suffice, but keep it explicit for readability:
  forM_ [1 .. nTrials] $ \_ -> trial
  examples <- readIORef ref
  printExamples examples
  pure examples

-- | Record one trace into the running collection: for every tag the
-- trace produced, keep this trace if it's the first or strictly
-- shorter than the previously stored one.
recordTrace :: IORef LabelledExamples -> [ModelStep] -> IO ()
recordTrace ref steps =
  let tagsHere = concatMap stepTags steps
      len      = length steps
      improve mb = case mb of
        Nothing                         -> Just steps
        Just prev | length prev > len   -> Just steps
                  | otherwise           -> mb
  in for_ tagsHere $ \tag ->
       modifyIORef' ref (Map.alter improve tag)

printExamples :: LabelledExamples -> IO ()
printExamples examples
  | Map.null examples =
      putStrLn "lockstepLabelledExamples: no tags emitted by lsCmdTag."
  | otherwise = do
      putStrLn $ "lockstepLabelledExamples: "
              <> show (Map.size examples)
              <> " tag(s) observed."
      forM_ (Map.toList examples) $ \(tag, steps) -> do
        putStrLn $ "Tag \"" <> tag <> "\" (" <> show (length steps) <> " action(s)):"
        forM_ steps $ \step -> do
          let tagSuffix = case stepTags step of
                [] -> ""
                ts -> "  [" <> commaJoin ts <> "]"
          putStrLn $ "  " <> stepRendered step <> tagSuffix
        putStrLn ""

commaJoin :: [String] -> String
commaJoin []       = ""
commaJoin [x]      = x
commaJoin (x : xs) = x <> ", " <> commaJoin xs

-- ---------------------------------------------------------------------------
-- Sampler: walks one model-only trace.
-- ---------------------------------------------------------------------------

-- | Generate one trace of up to @maxActions@ steps by walking the model.
sampleTrace
  :: forall m model.
     Int
  -> model
  -> [LockstepCmd m model]
  -> Gen [ModelStep]
sampleTrace maxActions model0 cmds = do
  n <- Gen.int (Range.linear 1 maxActions)
  go n (initialLockstepState model0)
  where
    go :: Int -> LockstepState model Symbolic -> Gen [ModelStep]
    go 0 _ = pure []
    go k st =
      case mapMaybe (`stepFor` st) cmds of
        []      -> pure []
        choices -> do
          stepGen <- Gen.element choices
          mResult <- stepGen
          case mResult of
            Nothing            -> pure []
            Just (st', step)   -> (step :) <$> go (k - 1) st'

-- | If @cmd@ is applicable in @st@, return a generator that produces
-- the post-step state and the rendered step (or 'Nothing' if the
-- precondition rejects the input post-generation).
stepFor
  :: forall m model.
     LockstepCmd m model
  -> LockstepState model Symbolic
  -> Maybe (Gen (Maybe (LockstepState model Symbolic, ModelStep)))
stepFor cmd st =
  case cmd of
    LockstepCmd lsCmdGen (_ :: input Hedgehog.Internal.State.Concrete -> m output)
                lsCmdModel lsCmdRequire _ _ lsCmdTag -> do
      inputGen <- lsCmdGen st
      Just $ do
        input <- inputGen
        if not (lsCmdRequire (lsModel st) input)
          then pure Nothing
          else do
            let varId           = lsNextVarId st
                var :: Var output Symbolic
                var             = Var (Symbolic (Name varId))
                (modelOut, m')  = lsCmdModel st input
                st'             = insertModelResult var modelOut (st { lsModel = m' })
                tags            = lsCmdTag (lsModel st) m' modelOut
                rendered        = show input
            pure (Just (st', ModelStep rendered tags))
