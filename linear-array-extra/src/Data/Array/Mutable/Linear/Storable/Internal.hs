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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

module Data.Array.Mutable.Linear.Storable.Internal (
  SArray (..),
  unsafeAlloc,
  unsafeAllocL,
  fromList,
  fromListL,
  unsafeSet,
  fill,
  size,
  unsafeGet,
  unsafeResize,
  unsafeSlice,
) where

import qualified Data.Array.Mutable.Linear.Class as C
import Data.Function (fix)
import Foreign
import Foreign.Marshal.Pure (MkRepresentable (..), Representable (..))
import GHC.Base (runRW#, unIO)
import Linear.Witness.Token (Linearly, linearly)
import Linear.Witness.Token.Unsafe (HasLinearWitness)
import Prelude.Linear
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

-- TODO: consider Pool-based variant?
data SArray a where
  SArray :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !(Ptr a) %1 -> SArray a
  deriving anyclass (HasLinearWitness)

instance Representable (SArray a) where
  type AsKnown (SArray a) = AsKnown (Int, Ptr a)

instance MkRepresentable (SArray a) (Int, Ptr a) where
  toRepr (SArray i p) = (i, p)
  {-# INLINE toRepr #-}
  ofRepr (i, p) = move i & \(Ur i) -> SArray i p
  {-# INLINE ofRepr #-}

{- | Note: 'Unbox'ed vectors should be regarded as an n-tuple of ByteArray;
So we can just drop it.
-}
instance Consumable (SArray a) where
  consume = Unsafe.toLinear \(SArray i mu) ->
    i `lseq` case runRW# (unIO (free mu)) of
      (# !_, () #) -> ()
  {-# NOINLINE consume #-}

unsafeStrictPerformIO :: IO a %1 -> a
{-# INLINE unsafeStrictPerformIO #-}
unsafeStrictPerformIO = Unsafe.toLinear \act -> case runRW# (unIO act) of
  (# !_, !a #) -> a

instance (Storable a) => Dupable (SArray a) where
  dup2 = Unsafe.toLinear \(SArray i mu) ->
    unsafeStrictPerformIO do
      mu' <- mallocArray i
      copyArray mu' mu i
      P.pure (SArray i mu, SArray i mu')
  {-# NOINLINE dup2 #-}

unsafeAlloc :: (Storable a) => Int -> (SArray a %1 -> Ur b) %1 -> Ur b
{-# NOINLINE unsafeAlloc #-}
unsafeAlloc n (f :: SArray a %1 -> b) =
  f (SArray n (unsafeStrictPerformIO $ mallocArray n))

unsafeAllocL :: (Storable a) => Int -> Linearly %1 -> SArray a
{-# NOINLINE unsafeAllocL #-}
unsafeAllocL n l =
  l `lseq` SArray n (unsafeStrictPerformIO $ mallocArray n)

size :: SArray a %1 -> (Ur Int, SArray a)
{-# INLINE size #-}
size (SArray i a) = (Ur i, SArray i a)

-- FIXME: more efficient implementation?
fill :: (Storable a) => a -> SArray a %1 -> SArray a
{-# NOINLINE fill #-}
fill a = Unsafe.toLinear \arr@(SArray n ptr) ->
  let go =
        fix
          ( \self !i ->
              if i == n
                then P.pure arr
                else pokeElemOff ptr i a P.>> self (i + 1)
          )
          0
   in unsafeStrictPerformIO go

fromListL :: (Storable a) => [a] -> Linearly %1 -> SArray a
{-# NOINLINE fromListL #-}
fromListL (xs :: [a]) l =
  l `lseq`
    let len = P.length xs
     in SArray len $ unsafeStrictPerformIO (newArray xs)

fromList :: (Storable a) => [a] -> (SArray a %1 -> Ur b) %1 -> Ur b
{-# INLINE fromList #-}
fromList xs f = linearly $ f . fromListL xs

unsafeSet :: (Storable a) => Int -> a -> SArray a %1 -> SArray a
{-# NOINLINE unsafeSet #-}
unsafeSet i a = Unsafe.toLinear \arr0@(SArray _ ptr) ->
  case unsafeStrictPerformIO (pokeElemOff ptr i a) of
    () -> arr0

unsafeGet :: (Storable a) => Int -> SArray a %1 -> (Ur a, SArray a)
{-# NOINLINE unsafeGet #-}
unsafeGet i = Unsafe.toLinear \arr0@(SArray _ ptr) ->
  case unsafeStrictPerformIO (peekElemOff ptr i) of
    a -> (Ur a, arr0)

unsafeResize :: (Storable a) => Int -> SArray a %1 -> SArray a
{-# NOINLINE unsafeResize #-}
unsafeResize n = Unsafe.toLinear \(SArray _ ptr) ->
  SArray n $ unsafeStrictPerformIO $ reallocArray ptr n

unsafeSlice :: (Storable a) => Int -> Int -> SArray a %1 -> (SArray a, SArray a)
{-# NOINLINE unsafeSlice #-}
unsafeSlice off len = Unsafe.toLinear \arr@(SArray _ ptr) ->
  unsafeStrictPerformIO do
    ptr' <- mallocArray len
    copyArray ptr' (advancePtr ptr off) len
    P.pure (arr, SArray len ptr')

instance (Storable a) => C.Array SArray a where
  unsafeAlloc = unsafeAlloc
  fromList = fromList
  fill = fill
  unsafeSet = unsafeSet
  size = size
  unsafeGet = unsafeGet
  unsafeSlice = unsafeSlice
  unsafeResize = unsafeResize
  unsafeAllocL = unsafeAllocL
