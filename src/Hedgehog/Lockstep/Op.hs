{-# LANGUAGE PolyKinds #-}
-- | Structural projections used with 'Hedgehog.Lockstep.GVar.GVar' to
-- extract components from compound action outputs.
--
-- The library ships a default 'Op' GADT covering pair, sum, identity, and
-- composition. Users who need additional projections (list head, record
-- field, etc.) can define their own GADT and provide an 'InterpretOp'
-- instance. 'Hedgehog.Lockstep.GVar.mkGVar' and
-- 'Hedgehog.Lockstep.GVar.mapGVar' accept any op with an 'InterpretOp' and
-- 'Show' instance.
--
-- For example, if an action returns @'Either' err ('Hedgehog.Internal.State.Var' h, name)@,
-- the projection @'OpRight' '>>>' 'OpFst'@ extracts the @h@.
module Hedgehog.Lockstep.Op
  ( -- * The default 'Op' GADT
    Op (..)
  , (>>>)

    -- * Extensibility
  , InterpretOp (..)
  , applyOp
  , showOp
  ) where

-- | Interpret a structural projection as a partial function on values.
--
-- Implementations return 'Nothing' for projections that don't apply to the
-- input value (e.g., 'OpLeft' applied to a 'Right'). Compose your own op
-- GADT and provide an instance to extend the projection vocabulary beyond
-- 'Op'. The library imposes no restriction on the kind or shape of @op@:
-- typical instances are GADTs of kind @'Data.Kind.Type' -> 'Data.Kind.Type' -> 'Data.Kind.Type'@.
class InterpretOp op where
  interpretOp :: op a b -> a -> Maybe b

-- | The default structural projections: identity, pair, sum, composition.
--
-- This covers the common cases of projecting from a tuple or @'Either'@.
-- For richer projections (list head, record field, custom decoders, etc.)
-- define your own op GADT and provide an 'InterpretOp' instance, then pass
-- it to 'Hedgehog.Lockstep.GVar.mkGVar' or
-- 'Hedgehog.Lockstep.GVar.mapGVar' just like 'Op'.
--
-- 'OpLeft' and 'OpRight' are partial: 'interpretOp' returns 'Nothing' if
-- the 'Either' doesn't match.
data Op a b where
  OpId    :: Op a a
  OpFst   :: Op (a, b) a
  OpSnd   :: Op (a, b) b
  OpLeft  :: Op (Either a b) a
  OpRight :: Op (Either a b) b
  OpComp  :: !(Op b c) -> !(Op a b) -> Op a c

instance InterpretOp Op where
  interpretOp OpId x = Just x
  interpretOp OpFst (a, _) = Just a
  interpretOp OpSnd (_, b) = Just b
  interpretOp OpLeft (Left a) = Just a
  interpretOp OpLeft (Right _) = Nothing
  interpretOp OpRight (Left _) = Nothing
  interpretOp OpRight (Right b) = Just b
  interpretOp (OpComp f g) x = interpretOp g x >>= interpretOp f
  {-# INLINABLE interpretOp #-}

instance Show (Op a b) where
  show OpId         = "id"
  show OpFst        = "fst"
  show OpSnd        = "snd"
  show OpLeft       = "left"
  show OpRight      = "right"
  show (OpComp f g) = show g <> "." <> show f

-- | Left-to-right composition of 'Op's.
(>>>) :: Op a b -> Op b c -> Op a c
f >>> g = OpComp g f
infixr 1 >>>

-- | Apply a structural projection. Synonym for 'interpretOp' kept for the
-- pre-0.2 name.
applyOp :: InterpretOp op => op a b -> a -> Maybe b
applyOp = interpretOp
{-# INLINE applyOp #-}

-- | Render an op as a string. Synonym for 'show' kept for the pre-0.2 name.
showOp :: Show (op a b) => op a b -> String
showOp = show
{-# INLINE showOp #-}
