{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

module Data.Array.Mutable.Linear.Unboxed (
  UArray (),
  Unbox,
  alloc,
  unsafeAlloc,
  unsafeAllocBeside,
  fill,
  unsafeSet,
  set,
  size,
  unsafeGet,
  freeze,
  unsafeSlice,
  unsafeResize,
) where

import qualified Data.Array.Mutable.Linear.Class as C
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified GHC.Base as GHC
import GHC.Stack (HasCallStack)
import Prelude.Linear
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

newtype UArray a = UArray (U.MVector GHC.RealWorld a)

alloc :: (HasCallStack, U.Unbox a) => Int -> a -> (UArray a %1 -> Ur b) %1 -> Ur b
{-# NOINLINE alloc #-}
alloc n x f
  | n < 0 = error ("UArray.alloc: Negative length: " <> show n) f
  | otherwise = case GHC.runRW# (GHC.unIO (MU.replicate n x)) of
      (# _, arr #) -> f (UArray arr)

unsafeAlloc :: U.Unbox a => Int -> (UArray a %1 -> Ur b) %1 -> Ur b
{-# NOINLINE unsafeAlloc #-}
unsafeAlloc n f = case GHC.runRW# (GHC.unIO (MU.new n)) of
  (# _, arr #) -> f (UArray arr)

unsafeAllocBeside :: U.Unbox a => Int -> UArray b %1 -> (UArray a, UArray b)
{-# NOINLINE unsafeAllocBeside #-}
unsafeAllocBeside n (UArray orig) =
  case GHC.runRW# (GHC.unIO (MU.new n)) of
    (# _, arr #) -> (UArray arr, UArray orig)

fill :: U.Unbox a => a -> UArray a %1 -> UArray a
{-# NOINLINE fill #-}
fill a = Unsafe.toLinear \(UArray arr) ->
  case GHC.runRW# (GHC.unIO (MU.set arr a)) of
    (# _, () #) -> UArray arr

-- | Check if given index is within the Array, otherwise panic.
assertIndexInRange :: (HasCallStack, Unbox a) => Int -> UArray a %1 -> UArray a
assertIndexInRange i arr =
  size arr & \(Ur s, arr') ->
    if 0 <= i && i < s
      then arr'
      else error "Array: index out of bounds" arr'

size :: U.Unbox a => UArray a %1 -> (Ur Int, UArray a)
size = Unsafe.toLinear \(UArray mu) ->
  (Ur (MU.length mu), UArray mu)

set :: (HasCallStack, U.Unbox a) => Int -> a -> UArray a %1 -> UArray a
set i x arr =
  unsafeSet i x (assertIndexInRange i arr)

unsafeSet :: U.Unbox a => Int -> a -> UArray a %1 -> UArray a
{-# NOINLINE unsafeSet #-}
unsafeSet i a = Unsafe.toLinear \(UArray arr) ->
  case GHC.runRW# (GHC.unIO (MU.write arr i a)) of
    (# _, () #) -> UArray arr

unsafeGet :: U.Unbox a => Int -> UArray a %1 -> (Ur a, UArray a)
{-# NOINLINE unsafeGet #-}
unsafeGet i = Unsafe.toLinear \(UArray mu) ->
  case GHC.runRW# (GHC.unIO (MU.read mu i)) of
    (# _, a #) -> (Ur a, UArray mu)

freeze :: U.Unbox a => UArray a %1 -> Ur (U.Vector a)
freeze = Unsafe.toLinear \(UArray arr) ->
  case GHC.runRW# (GHC.unIO (U.unsafeFreeze arr)) of
    (# _, u #) -> Ur u

unsafeSlice :: (U.Unbox a) => Int -> Int -> UArray a %1 -> (UArray a, UArray a)
{-# NOINLINE unsafeSlice #-}
unsafeSlice start len = Unsafe.toLinear \(UArray mu) ->
  case GHC.runRW# (GHC.unIO (MU.clone (MU.unsafeSlice start len mu))) of
    (# _, mu' #) -> (UArray mu, UArray mu')

unsafeResize :: (U.Unbox a) => Int -> UArray a %1 -> UArray a
{-# NOINLINE unsafeResize #-}
unsafeResize n = Unsafe.toLinear \(UArray arr) ->
  case compare n (MU.length arr) of
    LT -> case GHC.runRW# P.$ GHC.unIO P.$ MU.clone P.$ MU.unsafeTake n arr of
      (# _, arr #) -> UArray arr
    EQ -> UArray arr
    GT ->
      case GHC.runRW# P.$ GHC.unIO P.$ MU.unsafeGrow arr (n - MU.length arr) of
        (# _, arr #) -> UArray arr

{- | Note: 'Unbox'ed vectors should be regarded as an n-tuple of ByteArray;
So we can just drop it.
-}
instance (U.Unbox a) => Consumable (UArray a) where
  consume = Unsafe.toLinear \(UArray mu) ->
    unsafeDupablePerformIO (U.unsafeFreeze mu) `seq` ()
  {-# NOINLINE consume #-}

instance (U.Unbox a) => Dupable (UArray a) where
  dup2 = Unsafe.toLinear \(UArray mu) ->
    (UArray mu, UArray (unsafeDupablePerformIO (MU.clone mu)))
  {-# NOINLINE dup2 #-}

instance (U.Unbox a, Consumable a) => C.Array UArray a where
  unsafeAlloc = unsafeAlloc
  unsafeAllocBeside = unsafeAllocBeside
  fill = fill
  unsafeSet = unsafeSet
  size = size
  unsafeGet = unsafeGet
  unsafeSlice = unsafeSlice
  unsafeResize = unsafeResize
