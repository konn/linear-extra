{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

module Data.Array.Mutable.Linear.Unboxed (
  UArray (),
  Unbox,
  alloc,
  allocL,
  unsafeAlloc,
  unsafeAllocL,
  unsafeAllocBeside,
  fromList,
  fromListL,
  fromVectorL,
  fill,
  unsafeSet,
  set,
  size,
  unsafeGet,
  get,
  freeze,
  unsafeSlice,
  unsafeResize,
  map,
  mapSame,
  findIndex,
) where

import Data.Array.Mutable.Linear.Unboxed.Internal
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Exts (runRW#)
import qualified GHC.Exts as GHC
import GHC.IO (unIO)
import GHC.Stack (HasCallStack)
import Linear.Witness.Token
import Prelude.Linear hiding (map)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

alloc :: (HasCallStack, U.Unbox a) => Int -> a -> (UArray a %1 -> Ur b) %1 -> Ur b
{-# NOINLINE alloc #-}
alloc n x f
  | n < 0 = error ("UArray.alloc: Negative length: " <> show n) f
  | otherwise = case runRW# (unIO $ MU.replicate n x) of
      (# _, mu #) -> f (UArray mu)

allocL :: (HasCallStack, U.Unbox a) => Linearly %1 -> Int -> a -> UArray a
{-# NOINLINE allocL #-}
allocL = GHC.noinline \l n x ->
  if n < 0
    then error ("UArray.alloc: Negative length: " <> show n) l
    else
      l `lseq` case runRW# (unIO $ MU.replicate n x) of
        (# _, mu #) -> UArray mu

unsafeAllocBeside :: (U.Unbox a) => Int -> UArray b %1 -> (UArray a, UArray b)
{-# NOINLINE unsafeAllocBeside #-}
unsafeAllocBeside n (UArray orig) =
  case runRW# (unIO $ MU.unsafeNew n) of
    (# _, mu #) -> (UArray mu, UArray orig)

-- | Check if given index is within the Array, otherwise panic.
assertIndexInRange :: (HasCallStack, Unbox a) => Int -> UArray a %1 -> UArray a
assertIndexInRange i arr =
  size arr & \(Ur s, arr') ->
    if 0 <= i && i < s
      then arr'
      else error ("Unboxed Array: index out of bounds " <> show (i, s)) arr'

set :: (HasCallStack, U.Unbox a) => Int -> a -> UArray a %1 -> UArray a
set i x arr =
  unsafeSet i x (assertIndexInRange i arr)

get :: (HasCallStack, U.Unbox a) => Int -> UArray a %1 -> (Ur a, UArray a)
get i arr = unsafeGet i (assertIndexInRange i arr)

-- | /O(1)/ freeze
freeze :: (U.Unbox a) => UArray a %1 -> Ur (U.Vector a)
{-# NOINLINE freeze #-}
freeze = Unsafe.toLinear \(UArray mu) ->
  case runRW# $ unIO $ U.unsafeFreeze mu of
    (# _, uv #) -> Ur uv

fromVectorL :: (U.Unbox a) => U.Vector a %1 -> Linearly %1 -> UArray a
{-# NOINLINE fromVectorL #-}
fromVectorL = GHC.noinline $ Unsafe.toLinear \uv l ->
  case runRW# (unIO (U.unsafeThaw uv)) of
    (# _, mu #) -> l `lseq` UArray mu

fromListL :: (U.Unbox a) => [a] -> Linearly %1 -> UArray a
fromListL (xs :: [a]) =
  let len = P.length xs
   in go 0 xs . unsafeAllocL len
  where
    go :: Int -> [a] -> UArray a %1 -> UArray a
    go !_ [] arr = arr
    go !i (x : xs) arr =
      go (i + 1) xs (unsafeSet i x arr)

map :: (U.Unbox a, U.Unbox b) => (a -> b) -> UArray a %1 -> UArray b
{-# INLINE [1] map #-}
map (f :: a -> b) arr =
  size arr & \(Ur sz, arr) ->
    unsafeAllocBeside sz arr & \(dst, src) -> go 0 sz src dst
  where
    go :: Int -> Int -> UArray a %1 -> UArray b %1 -> UArray b
    go !i !j src dst
      | i == j = src `lseq` dst
      | otherwise =
          unsafeGet i src & \(Ur a, src) ->
            unsafeSet i (f a) dst & \dst ->
              go (i + 1) j src dst

mapSame :: (U.Unbox a) => (a -> a) -> UArray a %1 -> UArray a
mapSame (f :: a -> b) arr =
  size arr & \(Ur sz, src) -> go 0 sz src
  where
    go :: Int -> Int -> UArray a %1 -> UArray a
    go !i !j src
      | i == j = src
      | otherwise =
          unsafeGet i src & \(Ur a, src) ->
            unsafeSet i (f a) src & \src ->
              go (i + 1) j src

{-# RULES "map/mapSame" map = mapSame #-}

findIndex :: (U.Unbox a) => (a -> Bool) -> UArray a %1 -> (Ur (Maybe Int), UArray a)
{-# INLINE findIndex #-}
findIndex (p :: a -> Bool) arr = size arr & \(Ur sz, arr) -> loop 0 sz arr
  where
    loop :: Int -> Int -> UArray a %1 -> (Ur (Maybe Int), UArray a)
    loop !i !sz arr
      | i == sz = (Ur Nothing, arr)
      | otherwise =
          unsafeGet i arr & \(Ur a, arr) ->
            if p a
              then (Ur (Just i), arr)
              else loop (i + 1) sz arr
