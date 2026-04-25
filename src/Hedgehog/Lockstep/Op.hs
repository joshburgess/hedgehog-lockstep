module Hedgehog.Lockstep.Op
  ( Op (..)
  , applyOp
  , showOp
  , (>>>)
  ) where

-- | Structural projections for extracting components from compound types.
--
-- Used with 'GVar' to project model values from composite action outputs.
-- For example, if an action returns @Either Err (Handle, FilePath)@, you can
-- compose @OpRight '>>>' OpFst@ to extract the @Handle@.
--
-- 'OpLeft' and 'OpRight' are partial projections: 'applyOp' returns
-- 'Nothing' if the 'Either' doesn't match.
data Op a b where
  OpId    :: Op a a
  OpFst   :: Op (a, b) a
  OpSnd   :: Op (a, b) b
  OpLeft  :: Op (Either a b) a
  OpRight :: Op (Either a b) b
  OpComp  :: !(Op b c) -> !(Op a b) -> Op a c

-- | Apply an 'Op' projection. Returns 'Nothing' for failed partial
-- projections ('OpLeft' on 'Right', 'OpRight' on 'Left').
applyOp :: Op a b -> a -> Maybe b
applyOp OpId x = Just x
applyOp OpFst (a, _) = Just a
applyOp OpSnd (_, b) = Just b
applyOp OpLeft (Left a) = Just a
applyOp OpLeft (Right _) = Nothing
applyOp OpRight (Left _) = Nothing
applyOp OpRight (Right b) = Just b
applyOp (OpComp f g) x = applyOp g x >>= applyOp f

-- | Left-to-right composition of 'Op's.
(>>>) :: Op a b -> Op b c -> Op a c
f >>> g = OpComp g f
infixr 1 >>>

-- | Render an 'Op' as a human-readable projection chain.
showOp :: Op a b -> String
showOp OpId = "id"
showOp OpFst = "fst"
showOp OpSnd = "snd"
showOp OpLeft = "left"
showOp OpRight = "right"
showOp (OpComp f g) = showOp g <> "." <> showOp f

instance Show (Op a b) where
  show = showOp
