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

import qualified Control.Monad as P
import Control.Monad.ST.Strict (runST)
import Data.Alloc.Linearly.Token
import Data.Alloc.Linearly.Token.Unsafe (HasLinearWitness)
import qualified Data.Array.Mutable.Linear.Class as C
import Data.Coerce (coerce)
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Stack (HasCallStack)
import Prelude.Linear hiding (map)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

newtype UArray a = UArray (U.Vector a)
  deriving anyclass (HasLinearWitness)

alloc :: (HasCallStack, U.Unbox a) => Int -> a -> (UArray a %1 -> Ur b) %1 -> Ur b
alloc n x f
  | n < 0 = error ("UArray.alloc: Negative length: " <> show n) f
  | otherwise = f (UArray $ U.replicate n x)

allocL :: (HasCallStack, U.Unbox a) => Linearly %1 -> Int -> a -> UArray a
allocL l n x
  | n < 0 = error ("UArray.alloc: Negative length: " <> show n) l
  | otherwise =
      consume l & \() -> UArray (U.replicate n x)

unsafeAlloc :: U.Unbox a => Int -> (UArray a %1 -> Ur b) %1 -> Ur b
unsafeAlloc n (f :: UArray a %1 -> b) =
  f $ UArray $ runST (U.unsafeFreeze P.=<< MU.unsafeNew n)

unsafeAllocL :: U.Unbox a => Linearly %1 -> Int -> UArray a
unsafeAllocL l n = l `lseq` UArray (runST (U.unsafeFreeze P.=<< MU.unsafeNew n))

unsafeAllocBeside :: U.Unbox a => Int -> UArray b %1 -> (UArray a, UArray b)
unsafeAllocBeside n (UArray orig) =
  (UArray $ runST (U.unsafeFreeze P.=<< MU.unsafeNew n), UArray orig)

fill :: U.Unbox a => a -> UArray a %1 -> UArray a
fill a = Unsafe.toLinear \(UArray arr) -> runST do
  flip MU.set a P.=<< U.unsafeThaw arr
  P.pure P.$ UArray arr

-- | Check if given index is within the Array, otherwise panic.
assertIndexInRange :: (HasCallStack, Unbox a) => Int -> UArray a %1 -> UArray a
assertIndexInRange i arr =
  size arr & \(Ur s, arr') ->
    if 0 <= i && i < s
      then arr'
      else error "Array: index out of bounds" arr'

size :: U.Unbox a => UArray a %1 -> (Ur Int, UArray a)
size = Unsafe.toLinear \(UArray mu) ->
  (Ur (U.length mu), UArray mu)

set :: (HasCallStack, U.Unbox a) => Int -> a -> UArray a %1 -> UArray a
set i x arr =
  unsafeSet i x (assertIndexInRange i arr)

unsafeSet :: U.Unbox a => Int -> a -> UArray a %1 -> UArray a
unsafeSet i a = Unsafe.toLinear \arr0@(UArray arr) -> runST do
  mu <- U.unsafeThaw arr
  MU.write mu i a
  P.pure arr0

get :: (HasCallStack, U.Unbox a) => Int -> UArray a %1 -> (Ur a, UArray a)
get i arr = unsafeGet i (assertIndexInRange i arr)

unsafeGet :: U.Unbox a => Int -> UArray a %1 -> (Ur a, UArray a)
unsafeGet i = Unsafe.toLinear \(UArray mu) ->
  (Ur (U.unsafeIndex mu i), UArray mu)

-- | /O(1)/ freeze
freeze :: UArray a %1 -> Ur (U.Vector a)
freeze = Unsafe.toLinear (Ur P.. coerce)

unsafeSlice :: (U.Unbox a) => Int -> Int -> UArray a %1 -> (UArray a, UArray a)
{-# NOINLINE unsafeSlice #-}
unsafeSlice start len = Unsafe.toLinear \(UArray mu) ->
  (UArray mu, UArray $! U.force $! U.unsafeSlice start len mu)

unsafeResize :: (U.Unbox a) => Int -> UArray a %1 -> UArray a
{-# NOINLINE unsafeResize #-}
unsafeResize n = Unsafe.toLinear \(UArray arr) ->
  case compare n (U.length arr) of
    LT -> UArray $! U.force P.$! U.unsafeTake n arr
    EQ -> UArray arr
    GT -> runST do
      marr <- U.unsafeThaw arr
      marr <- MU.unsafeGrow marr (n - MU.length marr)
      UArray P.<$!> U.unsafeFreeze marr

{- | Note: 'Unbox'ed vectors should be regarded as an n-tuple of ByteArray;
So we can just drop it.
-}
instance Consumable (UArray a) where
  consume = Unsafe.toLinear \(UArray mu) -> mu `seq` ()
  {-# NOINLINE consume #-}

instance (U.Unbox a) => Dupable (UArray a) where
  dup2 = Unsafe.toLinear \(UArray mu) ->
    (UArray mu, UArray $! U.force mu)
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
