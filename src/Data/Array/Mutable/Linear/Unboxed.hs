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

import Data.Alloc.Linearly.Token
import Data.Alloc.Linearly.Token.Unsafe (HasLinearWitness)
import qualified Data.Array.Mutable.Linear.Class as C
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Exts (RealWorld, runRW#)
import GHC.IO (unIO)
import GHC.Stack (HasCallStack)
import Prelude.Linear hiding (map)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

newtype UArray a = UArray (MU.MVector RealWorld a)
  deriving anyclass (HasLinearWitness)

alloc :: (HasCallStack, U.Unbox a) => Int -> a -> (UArray a %1 -> Ur b) %1 -> Ur b
{-# NOINLINE alloc #-}
alloc n x f
  | n < 0 = error ("UArray.alloc: Negative length: " <> show n) f
  | otherwise = case runRW# (unIO $ MU.replicate n x) of
      (# _, mu #) -> f (UArray mu)

allocL :: (HasCallStack, U.Unbox a) => Linearly %1 -> Int -> a -> UArray a
{-# NOINLINE allocL #-}
allocL l n x
  | n < 0 = error ("UArray.alloc: Negative length: " <> show n) l
  | otherwise =
      consume l & \() -> case runRW# (unIO $ MU.replicate n x) of
        (# _, mu #) -> UArray mu

unsafeAlloc :: U.Unbox a => Int -> (UArray a %1 -> Ur b) %1 -> Ur b
{-# NOINLINE unsafeAlloc #-}
unsafeAlloc n (f :: UArray a %1 -> b) =
  case runRW# (unIO $ MU.unsafeNew n) of
    (# _, mu #) -> f (UArray mu)

unsafeAllocL :: U.Unbox a => Linearly %1 -> Int -> UArray a
{-# NOINLINE unsafeAllocL #-}
unsafeAllocL l n =
  l `lseq` case runRW# (unIO $ MU.unsafeNew n) of
    (# _, mu #) -> UArray mu

unsafeAllocBeside :: U.Unbox a => Int -> UArray b %1 -> (UArray a, UArray b)
{-# NOINLINE unsafeAllocBeside #-}
unsafeAllocBeside n (UArray orig) =
  case runRW# (unIO $ MU.unsafeNew n) of
    (# _, mu #) -> (UArray mu, UArray orig)

fill :: U.Unbox a => a -> UArray a %1 -> UArray a
{-# NOINLINE fill #-}
fill a = Unsafe.toLinear \(UArray arr) ->
  case runRW# $ unIO $ MU.set arr a of
    (# _, () #) -> UArray arr

-- | Check if given index is within the Array, otherwise panic.
assertIndexInRange :: (HasCallStack, Unbox a) => Int -> UArray a %1 -> UArray a
assertIndexInRange i arr =
  size arr & \(Ur s, arr') ->
    if 0 <= i && i < s
      then arr'
      else error ("Unboxed Array: index out of bounds " <> show (i, s)) arr'

size :: U.Unbox a => UArray a %1 -> (Ur Int, UArray a)
size = Unsafe.toLinear \(UArray mu) ->
  (Ur (MU.length mu), UArray mu)

set :: (HasCallStack, U.Unbox a) => Int -> a -> UArray a %1 -> UArray a
set i x arr =
  unsafeSet i x (assertIndexInRange i arr)

unsafeSet :: U.Unbox a => Int -> a -> UArray a %1 -> UArray a
{-# NOINLINE unsafeSet #-}
unsafeSet i a = Unsafe.toLinear \arr0@(UArray mu) ->
  case runRW# (unIO $ MU.unsafeWrite mu i a) of
    (# _, () #) -> arr0

get :: (HasCallStack, U.Unbox a) => Int -> UArray a %1 -> (Ur a, UArray a)
get i arr = unsafeGet i (assertIndexInRange i arr)

unsafeGet :: U.Unbox a => Int -> UArray a %1 -> (Ur a, UArray a)
{-# NOINLINE unsafeGet #-}
unsafeGet i = Unsafe.toLinear \arr0@(UArray mu) ->
  case runRW# (unIO $ MU.unsafeRead mu i) of
    (# _, x #) -> (Ur x, arr0)

-- | /O(1)/ freeze
freeze :: U.Unbox a => UArray a %1 -> Ur (U.Vector a)
{-# NOINLINE freeze #-}
freeze = Unsafe.toLinear \(UArray mu) ->
  case runRW# $ unIO $ U.unsafeFreeze mu of
    (# _, uv #) -> Ur uv

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

fromList :: U.Unbox a => [a] -> (UArray a %1 -> Ur b) %1 -> Ur b
fromList (xs :: [a]) f =
  let len = P.length xs
   in unsafeAlloc len (f . go 0 xs)
  where
    go :: Int -> [a] -> UArray a %1 -> UArray a
    go !_ [] arr = arr
    go !i (x : xs) arr =
      go (i + 1) xs (unsafeSet i x arr)

fromVectorL :: U.Unbox a => Linearly %1 -> U.Vector a %1 -> UArray a
{-# NOINLINE fromVectorL #-}
fromVectorL l = Unsafe.toLinear \uv ->
  case runRW# $ unIO $ U.unsafeThaw uv of
    (# _, mu #) -> l `lseq` UArray mu

fromListL :: U.Unbox a => Linearly %1 -> [a] -> UArray a
fromListL l (xs :: [a]) =
  let len = P.length xs
   in go 0 xs (unsafeAllocL l len)
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

findIndex :: U.Unbox a => (a -> Bool) -> UArray a %1 -> (Ur (Maybe Int), UArray a)
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
