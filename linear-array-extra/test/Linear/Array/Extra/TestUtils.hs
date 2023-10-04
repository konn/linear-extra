{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Linear.Array.Extra.TestUtils (
  module Linear.Witness.Token.TestUtils,
) where

import qualified Data.Array.Mutable.Linear.Primitive as LPA
import qualified Data.Array.Mutable.Linear.Unboxed as LUA
import Data.Primitive (Prim)
import qualified Data.Vector.Mutable.Linear.Unboxed as LUV
import qualified Data.Vector.Unboxed as U
import Linear.Witness.Token.TestUtils
import Prelude.Linear (Ur (..))
import qualified Prelude.Linear as PL
import qualified Test.Falsify.Generator as F

instance (Prim a) => HasArrayOp a (LPA.PrimArray a) where
  applyArrayOp (Get i) = \xs -> LPA.get i xs PL.& \(Ur _, xs') -> xs'
  applyArrayOp (Set i x) = LPA.set i x
  applyArrayOp (Modify i (F.Fn f)) = \xs ->
    LPA.get i xs PL.& \(Ur x, xs') -> LPA.set i (f x) xs'
  applyArrayOp (Map (F.Fn f)) = LPA.map f

instance (U.Unbox a) => HasArrayOp a (LUA.UArray a) where
  applyArrayOp (Get i) = \xs -> LUA.get i xs PL.& \(Ur _, xs') -> xs'
  applyArrayOp (Set i x) = LUA.set i x
  applyArrayOp (Modify i (F.Fn f)) = \xs ->
    LUA.get i xs PL.& \(Ur x, xs') -> LUA.set i (f x) xs'
  applyArrayOp (Map (F.Fn f)) = LUA.map f

instance (U.Unbox a) => HasArrayOp a (LUV.Vector a) where
  applyArrayOp (Get i) = \xs -> LUV.get i xs PL.& \(Ur _, xs') -> xs'
  applyArrayOp (Set i x) = LUV.set i x
  applyArrayOp (Modify i (F.Fn f)) = LUV.modify_ f i
  applyArrayOp (Map (F.Fn f)) = PL.flip LUV.map f
