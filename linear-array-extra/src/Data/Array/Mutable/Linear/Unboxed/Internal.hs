{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

module Data.Array.Mutable.Linear.Unboxed.Internal (
  UArray (..),
  unsafeAlloc,
  fromList,
  unsafeSet,
  fill,
  size,
  unsafeGet,
  unsafeResize,
  unsafeSlice,
  unsafeAllocL,
) where

import Data.Alloc.Linearly.Token (Linearly)
import Data.Alloc.Linearly.Token.Unsafe (HasLinearWitness)
import qualified Data.Array.Mutable.Linear.Class as C
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Base (runRW#, unIO)
import GHC.Exts (RealWorld)
import Prelude.Linear
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

newtype UArray a = UArray (MU.MVector RealWorld a)
  deriving anyclass (HasLinearWitness)

{- | Note: 'Unbox'ed vectors should be regarded as an n-tuple of ByteArray;
So we can just drop it.
-}
instance Consumable (UArray a) where
  consume = Unsafe.toLinear \(UArray mu) -> mu `seq` ()
  {-# NOINLINE consume #-}

instance (U.Unbox a) => Dupable (UArray a) where
  dup2 = Unsafe.toLinear \(UArray mu) ->
    case runRW# $ unIO $ MU.clone mu of
      (# _, mu' #) -> (UArray mu, UArray mu')
  {-# NOINLINE dup2 #-}

instance (U.Unbox a) => C.Array UArray a where
  unsafeAlloc = unsafeAlloc
  fromList = fromList
  fill = fill
  unsafeSet = unsafeSet
  size = size
  unsafeGet = unsafeGet
  unsafeSlice = unsafeSlice
  unsafeResize = unsafeResize
  unsafeAllocL = unsafeAllocL

unsafeAlloc :: U.Unbox a => Int -> (UArray a %1 -> Ur b) %1 -> Ur b
{-# NOINLINE unsafeAlloc #-}
unsafeAlloc n (f :: UArray a %1 -> b) =
  case runRW# (unIO $ MU.unsafeNew n) of
    (# _, mu #) -> f (UArray mu)

fromList :: U.Unbox a => [a] -> (UArray a %1 -> Ur b) %1 -> Ur b
fromList (xs :: [a]) f =
  let len = P.length xs
   in unsafeAlloc len (f . go 0 xs)
  where
    go :: Int -> [a] -> UArray a %1 -> UArray a
    go !_ [] arr = arr
    go !i (x : xs) arr =
      go (i + 1) xs (unsafeSet i x arr)

fill :: U.Unbox a => a -> UArray a %1 -> UArray a
{-# NOINLINE fill #-}
fill a = Unsafe.toLinear \(UArray arr) ->
  case runRW# $ unIO $ MU.set arr a of
    (# _, () #) -> UArray arr

unsafeSet :: U.Unbox a => Int -> a -> UArray a %1 -> UArray a
{-# NOINLINE unsafeSet #-}
unsafeSet i a = Unsafe.toLinear \arr0@(UArray mu) ->
  case runRW# (unIO $ MU.unsafeWrite mu i a) of
    (# _, () #) -> arr0

size :: U.Unbox a => UArray a %1 -> (Ur Int, UArray a)
size = Unsafe.toLinear \(UArray mu) ->
  (Ur (MU.length mu), UArray mu)

unsafeGet :: U.Unbox a => Int -> UArray a %1 -> (Ur a, UArray a)
{-# NOINLINE unsafeGet #-}
unsafeGet i = Unsafe.toLinear \arr0@(UArray mu) ->
  case runRW# (unIO $ MU.unsafeRead mu i) of
    (# _, x #) -> (Ur x, arr0)

unsafeSlice :: (U.Unbox a) => Int -> Int -> UArray a %1 -> (UArray a, UArray a)
{-# NOINLINE unsafeSlice #-}
unsafeSlice start len = Unsafe.toLinear \(UArray mu) ->
  case runRW# $ unIO $ MU.clone $! MU.unsafeSlice start len mu of
    (# _, mu' #) -> (UArray mu, UArray mu')

unsafeResize :: (U.Unbox a) => Int -> UArray a %1 -> UArray a
{-# NOINLINE unsafeResize #-}
unsafeResize n = Unsafe.toLinear \(UArray arr) ->
  case compare n (MU.length arr) of
    LT ->
      case runRW# $ unIO $ MU.clone $ MU.unsafeTake n arr of
        (# _, arr #) -> UArray arr
    EQ -> UArray arr
    GT -> case runRW# $ unIO $ MU.unsafeGrow arr (n - MU.length arr) of
      (# _, arr #) -> UArray arr

unsafeAllocL :: U.Unbox a => Linearly %1 -> Int -> UArray a
{-# NOINLINE unsafeAllocL #-}
unsafeAllocL l n =
  l `lseq` case runRW# (unIO $ MU.unsafeNew n) of
    (# _, mu #) -> UArray mu
