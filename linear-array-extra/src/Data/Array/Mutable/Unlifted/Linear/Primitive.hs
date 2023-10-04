{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Array.Mutable.Unlifted.Linear.Primitive (
  Prim (..),
  PrimArray#,
  unPrimArray#,
  lseq,
  alloc,
  allocL,
  unsafeAlloc,
  unsafeAllocL,
  allocBeside,
  unsafeAllocBeside,
  size,
  get,
  set,
  fill,
  copyInto,
  map,
  toList,
  freeze,
  dup2,
) where

import Data.Alloc.Linearly.Token
import Data.Primitive (MutableByteArray#, PrimArray (..))
import Data.Primitive.Types
import GHC.Exts (Proxy#, (*#))
import qualified GHC.Exts as GHC
import Prelude.Linear hiding (dup2, lseq, map)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

{-# ANN module "HLint: ignore Redundant case" #-}

-- | A mutable array holding _primtive_ values @a@s.
newtype PrimArray# a = PrimArray# (MutableByteArray# GHC.RealWorld)

unPrimArray# :: (MutableByteArray# GHC.RealWorld -> b) -> PrimArray# a %1 -> Ur b
unPrimArray# f = Unsafe.toLinear $ \(PrimArray# a) -> Ur (f a)

lseq :: PrimArray# a %1 -> b %1 -> b
lseq = Unsafe.toLinear2 \_ b -> b

infixr 0 `lseq`

allocL :: forall a. Prim a => Linearly %1 -> Int -> a -> PrimArray# a
{-# NOINLINE allocL #-}
allocL tok (GHC.I# n#) a =
  let byteSize# = n# GHC.*# sizeOf# (undefined :: a)
      new = GHC.runRW# P.$ \s ->
        case GHC.newByteArray# byteSize# s of
          (# s, arr #) -> case setByteArray# arr 0# n# a s of
            !_ -> PrimArray# arr
   in consume tok & \() -> new

-- | _See also_: 'unsafeAlloc'
alloc :: forall a b. Prim a => Int -> a -> (PrimArray# a %1 -> Ur b) %1 -> Ur b
alloc (GHC.I# n#) a f =
  let byteSize# = n# GHC.*# sizeOf# (undefined :: a)
      new = GHC.runRW# P.$ \s ->
        case GHC.newByteArray# byteSize# s of
          (# s, arr #) -> case setByteArray# arr 0# n# a s of
            !_ -> PrimArray# arr
   in f new
{-# NOINLINE alloc #-} -- prevents runRW# from floating outwards

-- | Allocates primitive array but WITHOUT initialisation.
unsafeAlloc :: forall a b. Prim a => Int -> (PrimArray# a %1 -> Ur b) %1 -> Ur b
unsafeAlloc (GHC.I# n#) f =
  let byteSize# = n# GHC.*# sizeOf# (undefined :: a)
      new = GHC.runRW# P.$ \s ->
        case GHC.newByteArray# byteSize# s of
          (# _, arr #) -> PrimArray# arr
   in f new
{-# NOINLINE unsafeAlloc #-} -- prevents runRW# from floating outwards

-- | Allocates primitive array but WITHOUT initialisation.
unsafeAllocL :: forall a. Prim a => Linearly %1 -> Int -> PrimArray# a
unsafeAllocL l (GHC.I# n#) =
  let byteSize# = n# GHC.*# sizeOf# (undefined :: a)
      new = GHC.runRW# P.$ \s ->
        case GHC.newByteArray# byteSize# s of
          (# _, arr #) -> PrimArray# arr
   in consume l & \() -> new
{-# NOINLINE unsafeAllocL #-} -- prevents runRW# from floating outwards

fill :: Prim a => a -> PrimArray# a %1 -> PrimArray# a
fill (a :: a) = Unsafe.toLinear \(PrimArray# arr :: PrimArray# a) ->
  let !(GHC.I# i) = sizeOfPrimArray# (GHC.proxy# @a) arr
   in case GHC.runRW# (setByteArray# arr 0# i a) of
        !_ -> PrimArray# arr
{-# NOINLINE fill #-}

-- | _See also_: 'unsafeAllocBeside'
allocBeside :: forall a b. Prim a => Int -> a -> PrimArray# b %1 -> (# PrimArray# a, PrimArray# b #)
allocBeside (GHC.I# n#) a orig =
  let new = GHC.runRW# P.$ \s ->
        case GHC.newByteArray# (n# GHC.*# sizeOf# (undefined :: a)) s of
          (# s, arr #) -> case setByteArray# arr 0# n# a s of
            !_ -> PrimArray# arr
   in (# new, orig #)
{-# NOINLINE allocBeside #-} -- prevents runRW# from floating outwards

-- | Same as 'allocBeside', but without initialisation.
unsafeAllocBeside :: forall a b. Prim a => Int -> PrimArray# b %1 -> (# PrimArray# a, PrimArray# b #)
unsafeAllocBeside (GHC.I# n#) orig =
  let new = GHC.runRW# P.$ \s ->
        case GHC.newByteArray# (n# GHC.*# sizeOf# (undefined :: a)) s of
          (# _, arr #) -> PrimArray# arr
   in (# new, orig #)
{-# NOINLINE unsafeAllocBeside #-} -- prevents runRW# from floating outwards

size :: forall a. Prim a => PrimArray# a %1 -> (# Ur Int, PrimArray# a #)
size = Unsafe.toLinear \(PrimArray# arr) ->
  (# Ur (sizeOfPrimArray# (GHC.proxy# @a) arr), PrimArray# arr #)

sizeOfPrimArray# :: Prim a => Proxy# a -> MutableByteArray# GHC.RealWorld -> Int
sizeOfPrimArray# (_ :: Proxy# a) arr =
  GHC.I#
    ( GHC.sizeofMutableByteArray# arr
        `GHC.quotInt#` sizeOf# (undefined :: a)
    )

get :: forall a. Prim a => Int -> PrimArray# a %1 -> (# Ur a, PrimArray# a #)
get (GHC.I# i) = Unsafe.toLinear go
  where
    go :: PrimArray# a -> (# Ur a, PrimArray# a #)
    go (PrimArray# arr) =
      case GHC.runRW# (readByteArray# arr i) of
        (# _, ret #) -> (# Ur ret, PrimArray# arr #)
{-# NOINLINE get #-} -- prevents the runRW# effect from being reordered

set :: forall a. Prim a => Int -> a -> PrimArray# a %1 -> PrimArray# a
set (GHC.I# i) (a :: a) = Unsafe.toLinear go
  where
    go :: PrimArray# a -> PrimArray# a
    go (PrimArray# arr) =
      case GHC.runRW# (writeByteArray# arr i a) of
        !_ -> PrimArray# arr
{-# NOINLINE set #-} -- prevents the runRW# effect from being reordered

{- | Copy the first mutable array into the second mutable array, starting
from the given index of the source array.

It copies fewer elements if the second array is smaller than the
first. 'n' should be within [0..size src).

@
 copyInto n src dest:
  dest[i] = src[n+i] for i < size dest, i < size src + n
@
-}
copyInto ::
  forall a.
  Prim a =>
  Int ->
  PrimArray# a %1 ->
  PrimArray# a %1 ->
  (# PrimArray# a, PrimArray# a #)
copyInto start@(GHC.I# start#) = Unsafe.toLinear2 go
  where
    go :: PrimArray# a -> PrimArray# a -> (# PrimArray# a, PrimArray# a #)
    go (PrimArray# src) (PrimArray# dst) =
      let !(GHC.I# len#) =
            P.min
              (sizeOfPrimArray# (GHC.proxy# @a) src P.- start)
              (sizeOfPrimArray# (GHC.proxy# @a) dst)
          !sz# = sizeOf# (undefined :: a)
       in case GHC.runRW# (GHC.copyMutableByteArray# src (start# *# sz#) dst 0# (len# *# sz#)) of
            !_ -> (# PrimArray# src, PrimArray# dst #)
{-# NOINLINE copyInto #-} -- prevents the runRW# effect from being reordered

map :: (Prim a, Prim b) => (a -> b) -> PrimArray# a %1 -> PrimArray# b
map (f :: a -> b) =
  Unsafe.toLinear
    ( \(PrimArray# as) ->
        let len :: GHC.Int#
            !(GHC.I# len) = sizeOfPrimArray# (GHC.proxy# @a) as
         in GHC.runRW# \s ->
              case GHC.newByteArray# (len GHC.*# sizeOf# (undefined :: b)) s of
                (# s, bs #) ->
                  let
                    -- For each index ([0..len - 1]), we read the element on 'as', pass
                    -- it through 'f' and write to the same location on 'bs'.
                    go :: GHC.Int# -> GHC.State# GHC.RealWorld -> ()
                    go i st
                      | GHC.I# i P.== GHC.I# len = ()
                      | P.otherwise =
                          case readByteArray# as i st of
                            (# st', a #) ->
                              case writeByteArray# bs i (f a) st' of
                                !st'' -> go (i GHC.+# 1#) st''
                   in
                    go 0# s `GHC.seq` PrimArray# bs
    )
{-# NOINLINE map #-}

-- | Return the array elements as a lazy list.
toList :: forall a. Prim a => PrimArray# a %1 -> Ur [a]
toList = unPrimArray# P.$ \arr ->
  go
    0
    (sizeOfPrimArray# (GHC.proxy# @a) arr)
    []
    arr
  where
    go !i !len acc arr
      | i P.== len = acc
      | GHC.I# i# <- i =
          case GHC.runRW# (readByteArray# arr i#) of
            (# _, !ret #) -> go (i P.+ 1) len (ret : acc) arr

-- | /O(1)/ convert an 'PrimArray#' into immutable 'PrimArray'.
freeze :: (PrimArray a -> b) -> PrimArray# a %1 -> Ur b
freeze f = unPrimArray# go
  where
    go mut = case GHC.runRW# (GHC.unsafeFreezeByteArray# mut) of
      (# _, ret #) -> f (PrimArray ret)

dup2 :: PrimArray# a %1 -> (# PrimArray# a, PrimArray# a #)
dup2 = Unsafe.toLinear go
  where
    go :: PrimArray# a -> (# PrimArray# a, PrimArray# a #)
    go (PrimArray# arr) = GHC.runRW# \s ->
      let len# = GHC.sizeofMutableByteArray# arr
       in case GHC.newByteArray# len# s of
            (# s, new #) ->
              case GHC.copyMutableByteArray# arr 0# new 0# len# s of
                !_ -> (# PrimArray# arr, PrimArray# new #)
{-# NOINLINE dup2 #-}
