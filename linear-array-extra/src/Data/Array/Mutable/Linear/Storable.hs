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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

module Data.Array.Mutable.Linear.Storable (
  SArray,
  Storable,
  allocL,
  unsafeAlloc,
  unsafeAllocL,
  fromVectorL,
  fill,
  size,
  fromList,
  fromListL,
  freeze,
  set,
  unsafeSet,
  get,
  unsafeGet,
  unsafeResize,
  unsafeSlice,
  unsafeModify,
  unsafeModify_,
) where

import Data.Array.Mutable.Linear.Storable.Internal
import qualified Data.Vector.Storable as SV
import Foreign
import qualified GHC.Base as GHC
import GHC.Exts (noinline)
import GHC.Stack (HasCallStack)
import Linear.Witness.Token
import Prelude.Linear
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

{- | Allocate a constant array given a size and an initial value
The size must be non-negative, otherwise this errors.

/See also/: 'unsafeAllocL'
-}
allocL :: (HasCallStack, Storable a) => Int -> a -> Linearly %1 -> SArray a
{-# INLINE allocL #-}
allocL s x l
  | s P.< 0 = l `lseq` error ("allocL: negative length: " <> show s)
  | otherwise = fill x (unsafeAllocL s l)

fromVectorL :: (Storable a) => SV.Vector a -> Linearly %1 -> SArray a
{-# NOINLINE fromVectorL #-}
fromVectorL =
  noinline
    ( Unsafe.toLinear2 \v l ->
        unsafeAllocL (SV.length v) l & \(SArray sz ptr) ->
          case GHC.runRW# $ GHC.unIO $ SV.unsafeWith v $ \src -> Unsafe.toLinear copyArray ptr src sz of
            (# !_, () #) -> SArray sz ptr
    )

freeze :: (Storable a) => SArray a %1 -> Ur (SV.Vector a)
{-# NOINLINE freeze #-}
freeze = noinline $ Unsafe.toLinear \(SArray l ptr) ->
  case GHC.runRW# (GHC.unIO (newForeignPtr finalizerFree ptr)) of
    (# !_, fptr #) -> Ur (SV.unsafeFromForeignPtr0 fptr l)

set :: (HasCallStack, Storable a) => Int -> a -> SArray a %1 -> SArray a
{-# INLINE set #-}
set i a v =
  size v & \(Ur sz, v) ->
    if 0 <= i && i < sz
      then unsafeSet i a v
      else v `lseq` error ("set: out of bounds: " <> show (i, sz))

get :: (HasCallStack, Storable a) => Int -> SArray a %1 -> (Ur a, SArray a)
{-# INLINE get #-}
get i v =
  size v & \(Ur sz, v) ->
    if 0 <= i && i < sz
      then unsafeGet i v
      else v `lseq` error ("get: out of bounds: " <> show (i, sz))

-- | _Non-atomically_ modifies the content of
unsafeModify :: (Storable a) => (a -> (a, b)) -> Int -> SArray a %1 -> (Ur b, SArray a)
{-# INLINE unsafeModify #-}
unsafeModify f i v =
  unsafeGet i v & \(Ur a, v) ->
    f a & \(a, b) ->
      (Ur b, unsafeSet i a v)

-- | _Non-atomically_ modifies the content of
unsafeModify_ :: (Storable a) => (a -> a) -> Int -> SArray a %1 -> SArray a
{-# INLINE unsafeModify_ #-}
unsafeModify_ f i v =
  unsafeGet i v & \(Ur a, v) ->
    unsafeSet i (f a) v
