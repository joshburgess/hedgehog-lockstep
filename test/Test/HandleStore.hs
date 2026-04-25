module Test.HandleStore
  ( prop_handleSequential
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hedgehog.Lockstep

-- ---------------------------------------------------------------------------
-- System under test: a mutable handle-based store
--
-- Open returns Either String (Handle, Name). Subsequent Read/Write/Close
-- must extract the Handle from a previous Open via GVar projections:
-- OpRight >>> OpFst
-- ---------------------------------------------------------------------------

type Handle = Int
type Name = String
type Content = String

data RealStore = RealStore
  { rsNextHandle :: {-# UNPACK #-} !Int
  , rsHandles    :: !(Map Handle (Name, Content))
  }

newRealStore :: IO (IORef RealStore)
newRealStore = newIORef (RealStore 0 Map.empty)

rsOpen :: IORef RealStore -> Name -> IO (Either String (Handle, Name))
rsOpen ref name = do
  st <- readIORef ref
  let inUse = any (\(n, _) -> n == name) (rsHandles st)
  if inUse
    then pure (Left ("already open: " <> name))
    else do
      let h = rsNextHandle st
      writeIORef ref st
        { rsNextHandle = h + 1
        , rsHandles = Map.insert h (name, "") (rsHandles st)
        }
      pure (Right (h, name))

rsWrite :: IORef RealStore -> Handle -> Content -> IO (Either String ())
rsWrite ref h content = do
  st <- readIORef ref
  case Map.lookup h (rsHandles st) of
    Nothing -> pure (Left "bad handle")
    Just (name, _) -> do
      writeIORef ref st { rsHandles = Map.insert h (name, content) (rsHandles st) }
      pure (Right ())

rsRead :: IORef RealStore -> Handle -> IO (Either String Content)
rsRead ref h = do
  st <- readIORef ref
  pure $ case Map.lookup h (rsHandles st) of
    Nothing           -> Left "bad handle"
    Just (_, content) -> Right content

rsClose :: IORef RealStore -> Handle -> IO (Either String ())
rsClose ref h = do
  st <- readIORef ref
  case Map.lookup h (rsHandles st) of
    Nothing -> pure (Left "bad handle")
    Just _  -> do
      writeIORef ref st { rsHandles = Map.delete h (rsHandles st) }
      pure (Right ())

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

data Model = Model
  { mNextHandle :: !Int
  , mHandles    :: !(Map Handle (Name, Content))
  } deriving stock (Show)

initialModel :: Model
initialModel = Model 0 Map.empty

mOpen :: Model -> Name -> (Either String (Handle, Name), Model)
mOpen m name =
  let inUse = any (\(n, _) -> n == name) (mHandles m)
  in if inUse
     then (Left ("already open: " <> name), m)
     else
       let h = mNextHandle m
           m' = m { mNextHandle = h + 1
                   , mHandles = Map.insert h (name, "") (mHandles m)
                   }
       in (Right (h, name), m')

mWrite :: Model -> Handle -> Content -> (Either String (), Model)
mWrite m h content =
  case Map.lookup h (mHandles m) of
    Nothing -> (Left "bad handle", m)
    Just (name, _) ->
      (Right (), m { mHandles = Map.insert h (name, content) (mHandles m) })

mRead :: Model -> Handle -> (Either String Content, Model)
mRead m h =
  case Map.lookup h (mHandles m) of
    Nothing           -> (Left "bad handle", m)
    Just (_, content) -> (Right content, m)

mClose :: Model -> Handle -> (Either String (), Model)
mClose m h =
  case Map.lookup h (mHandles m) of
    Nothing -> (Left "bad handle", m)
    Just _  -> (Right (), m { mHandles = Map.delete h (mHandles m) })

-- ---------------------------------------------------------------------------
-- Inputs (barbies-style HKD)
-- ---------------------------------------------------------------------------

data OpenInput v = OpenInput !Name
  deriving stock (Show)

instance FunctorB OpenInput where bmap _ (OpenInput n) = OpenInput n
instance TraversableB OpenInput where btraverse _ (OpenInput n) = pure (OpenInput n)

data WriteInput v = WriteInput !(GVar Handle v) !Content
  deriving stock (Show)

instance FunctorB WriteInput where
  bmap f (WriteInput gv c) = WriteInput (bmap f gv) c
instance TraversableB WriteInput where
  btraverse f (WriteInput gv c) = (\gv' -> WriteInput gv' c) <$> btraverse f gv

data ReadInput v = ReadInput !(GVar Handle v)
  deriving stock (Show)

instance FunctorB ReadInput where
  bmap f (ReadInput gv) = ReadInput (bmap f gv)
instance TraversableB ReadInput where
  btraverse f (ReadInput gv) = ReadInput <$> btraverse f gv

data CloseInput v = CloseInput !(GVar Handle v)
  deriving stock (Show)

instance FunctorB CloseInput where
  bmap f (CloseInput gv) = CloseInput (bmap f gv)
instance TraversableB CloseInput where
  btraverse f (CloseInput gv) = CloseInput <$> btraverse f gv

-- ---------------------------------------------------------------------------
-- GVar helpers
-- ---------------------------------------------------------------------------

type OpenResult = Either String (Handle, Name)

-- | Pick a handle GVar: projects Open's result via OpRight >>> OpFst.
pickHandle :: LockstepState Model Symbolic -> Gen (GVar Handle Symbolic)
pickHandle st = do
  let vars = varsOfType @OpenResult st
  var <- Gen.element vars
  let op :: Op OpenResult Handle
      op = OpRight >>> OpFst
  pure $ mkGVar var op

resolveHandle :: Ord1 v => GVar Handle v -> LockstepState Model v -> Maybe Handle
resolveHandle gv st = resolveGVar gv (getEntries st)

-- ---------------------------------------------------------------------------
-- Commands
-- ---------------------------------------------------------------------------

genName :: Gen Name
genName = Gen.element ["alpha", "beta", "gamma"]

cmdOpen :: IORef RealStore -> LockstepCmd (PropertyT IO) Model
cmdOpen ref = LockstepCmd
  { lsCmdGen = \_ -> Just $ OpenInput <$> genName

  , lsCmdExec = \(OpenInput name) -> evalIO $ rsOpen ref name

  , lsCmdModel = \st (OpenInput name) ->
      mOpen (getModel st) name

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \modelResult realResult ->
      fmap (const ()) modelResult === fmap (const ()) realResult

  , lsCmdInvariants = \_ _ -> pure ()

  , lsCmdTag = \_ _ _ -> []
  }

cmdWrite :: IORef RealStore -> LockstepCmd (PropertyT IO) Model
cmdWrite ref = LockstepCmd
  { lsCmdGen = \st ->
      if null (varsOfType @OpenResult st)
        then Nothing
        else Just $ WriteInput <$> pickHandle st
                               <*> Gen.string (Range.linear 0 20) Gen.alpha

  , lsCmdExec = \(WriteInput gv content) ->
      case concreteGVar gv of
        Just h  -> evalIO $ rsWrite ref h content
        Nothing -> pure (Left "bad handle")

  , lsCmdModel = \st (WriteInput gv content) ->
      case resolveHandle gv st of
        Just h  -> mWrite (getModel st) h content
        Nothing -> (Left "bad handle", getModel st)

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \expected actual -> expected === actual

  , lsCmdInvariants = \_ _ -> pure ()

  , lsCmdTag = \_ _ _ -> []
  }

cmdRead :: IORef RealStore -> LockstepCmd (PropertyT IO) Model
cmdRead ref = LockstepCmd
  { lsCmdGen = \st ->
      if null (varsOfType @OpenResult st)
        then Nothing
        else Just $ ReadInput <$> pickHandle st

  , lsCmdExec = \(ReadInput gv) ->
      case concreteGVar gv of
        Just h  -> evalIO $ rsRead ref h
        Nothing -> pure (Left "bad handle")

  , lsCmdModel = \st (ReadInput gv) ->
      case resolveHandle gv st of
        Just h  -> mRead (getModel st) h
        Nothing -> (Left "bad handle", getModel st)

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \expected actual -> expected === actual

  , lsCmdInvariants = \_ _ -> pure ()

  , lsCmdTag = \_ _ _ -> []
  }

cmdClose :: IORef RealStore -> LockstepCmd (PropertyT IO) Model
cmdClose ref = LockstepCmd
  { lsCmdGen = \st ->
      if null (varsOfType @OpenResult st)
        then Nothing
        else Just $ CloseInput <$> pickHandle st

  , lsCmdExec = \(CloseInput gv) ->
      case concreteGVar gv of
        Just h  -> evalIO $ rsClose ref h
        Nothing -> pure (Left "bad handle")

  , lsCmdModel = \st (CloseInput gv) ->
      case resolveHandle gv st of
        Just h  -> mClose (getModel st) h
        Nothing -> (Left "bad handle", getModel st)

  , lsCmdRequire = \_ _ -> True

  , lsCmdObserve = \expected actual -> expected === actual

  -- System invariant: closed handles never appear in the model's open set,
  -- so the open-handle count never exceeds the next-handle counter.
  , lsCmdInvariants = \model _ ->
      assert (Map.size (mHandles model) <= mNextHandle model)

  , lsCmdTag = \_ _ _ -> []
  }

-- ---------------------------------------------------------------------------
-- Property
-- ---------------------------------------------------------------------------

prop_handleSequential :: Property
prop_handleSequential =
  lockstepPropertyWith
    initialModel
    50
    newRealStore
    (\ref -> writeIORef ref (RealStore 0 Map.empty))
    (\ref -> [cmdOpen ref, cmdWrite ref, cmdRead ref, cmdClose ref])
