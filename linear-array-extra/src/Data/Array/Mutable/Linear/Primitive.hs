{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

module Data.Array.Mutable.Linear.Primitive (
  -- * Mutable Linear Primitive Arrays
  PrimArray (..),

  -- * Performing Computations with Arrays
  alloc,
  allocL,
  unsafeAlloc,
  unsafeAllocL,
  allocBeside,
  unsafeAllocBeside,
  fromList,
  fromListL,

  -- * Modifications
  fill,
  set,
  unsafeSet,
  resize,
  unsafeResize,
  map,

  -- * Accessors
  get,
  unsafeGet,
  size,
  slice,
  unsafeSlice,
  toList,
  freeze,

  -- * Mutable-style interface
  read,
  unsafeRead,
  write,
  unsafeWrite,
) where

import qualified Data.Array.Mutable.Linear.Class as C
import Data.Array.Mutable.Unlifted.Linear.Primitive (PrimArray#)
import qualified Data.Array.Mutable.Unlifted.Linear.Primitive as Unlifted
import Data.Primitive (Prim)
import qualified Data.Primitive as Prim
import qualified Data.Vector.Primitive as PV
import GHC.Stack (HasCallStack)
import Linear.Witness.Token
import Linear.Witness.Token.Unsafe (HasLinearWitness)
import Prelude.Linear hiding (map, read)
import qualified Prelude as P

data PrimArray a = PrimArray (PrimArray# a)
  deriving anyclass (HasLinearWitness)

instance Consumable (PrimArray a) where
  consume :: PrimArray a %1 -> ()
  consume (PrimArray arr) = arr `Unlifted.lseq` ()

instance Dupable (PrimArray a) where
  dup2 :: PrimArray a %1 -> (PrimArray a, PrimArray a)
  dup2 (PrimArray arr) = wrap (Unlifted.dup2 arr)
    where
      wrap :: (# PrimArray# a, PrimArray# a #) %1 -> (PrimArray a, PrimArray a)
      wrap (# a1, a2 #) = (PrimArray a1, PrimArray a2)

{- | Allocate a constant array given a size and an initial value
The size must be non-negative, otherwise this errors.

/See also/: 'unsafeAlloc'
-}
alloc :: (HasCallStack, Prim a) => Int -> a -> (PrimArray a %1 -> Ur b) %1 -> Ur b
{-# ANN alloc "HLint: ignore Avoid lambda" #-}
alloc s x f
  | s < 0 = error "PrimArray.alloc: negative size" f
  | otherwise = Unlifted.alloc s x \arr# -> f (PrimArray arr#)

{- | Allocate a constant array given a size and an initial value
The size must be non-negative, otherwise this errors.

/See also/: 'unsafeAlloc'
-}
allocL :: (HasCallStack, Prim a) => Int -> a -> Linearly %1 -> PrimArray a
allocL s x l
  | s < 0 = error "PrimArray.alloc: negative size" l
  | otherwise = PrimArray (Unlifted.allocL s x l)

-- | Same as 'alloc', but without initial value.
unsafeAlloc :: (Prim a) => Int -> (PrimArray a %1 -> Ur b) %1 -> Ur b
{-# ANN unsafeAlloc "HLint: ignore Avoid lambda" #-}
unsafeAlloc s f = Unlifted.unsafeAlloc s \arr# -> f (PrimArray arr#)

-- | Same as 'alloc', but without initial value.
unsafeAllocL :: (Prim a) => Int -> Linearly %1 -> PrimArray a
unsafeAllocL s l = PrimArray (Unlifted.unsafeAllocL s l)

{- | Allocate a constant array given a size and an initial value,
using another array as a uniqueness proof.

/See also/: 'unsafeAllocBeside'
-}
allocBeside :: (HasCallStack, Prim a) => Int -> a -> PrimArray b %1 -> (PrimArray a, PrimArray b)
allocBeside s x (PrimArray orig)
  | s < 0 =
      Unlifted.lseq
        orig
        (error ("PrimArray.allocBeside: negative size: " ++ show s))
  | otherwise =
      wrap (Unlifted.allocBeside s x orig)
  where
    wrap :: (# PrimArray# a, PrimArray# b #) %1 -> (PrimArray a, PrimArray b)
    wrap (# orig, new #) = (PrimArray orig, PrimArray new)

unsafeAllocBeside :: (Prim a) => Int -> PrimArray b %1 -> (PrimArray a, PrimArray b)
unsafeAllocBeside s (PrimArray orig) = wrap (Unlifted.unsafeAllocBeside s orig)
  where
    wrap :: (# PrimArray# a, PrimArray# b #) %1 -> (PrimArray a, PrimArray b)
    wrap (# orig, new #) = (PrimArray orig, PrimArray new)

fromList ::
  (Prim a) =>
  [a] ->
  (PrimArray a %1 -> Ur b) %1 ->
  Ur b
fromList list (f :: PrimArray a %1 -> Ur b) =
  unsafeAlloc
    (P.length list)
    (f . insert)
  where
    insert :: PrimArray a %1 -> PrimArray a
    insert = doWrites (P.zip list [0 ..])

    doWrites :: [(a, Int)] -> PrimArray a %1 -> PrimArray a
    doWrites [] arr = arr
    doWrites ((a, ix) : xs) arr = doWrites xs (unsafeSet ix a arr)

fromListL ::
  (Prim a) =>
  [a] ->
  Linearly %1 ->
  PrimArray a
fromListL (list :: [a]) = insert . unsafeAllocL (P.length list)
  where
    insert :: PrimArray a %1 -> PrimArray a
    insert = doWrites (P.zip list [0 ..])

    doWrites :: [(a, Int)] -> PrimArray a %1 -> PrimArray a
    doWrites [] arr = arr
    doWrites ((a, ix) : xs) arr = doWrites xs (unsafeSet ix a arr)

size :: (Prim a) => PrimArray a %1 -> (Ur Int, PrimArray a)
size (PrimArray arr) = f (Unlifted.size arr)
  where
    f :: (# Ur Int, PrimArray# a #) %1 -> (Ur Int, PrimArray a)
    f (# s, arr #) = (s, PrimArray arr)

-- | Check if given index is within the Array, otherwise panic.
assertIndexInRange :: (HasCallStack, Prim a) => Int -> PrimArray a %1 -> PrimArray a
assertIndexInRange i arr =
  size arr & \(Ur s, arr') ->
    if 0 <= i && i < s
      then arr'
      else arr' `lseq` error ("Array: index out of bounds " <> show (i, s))

{- | Sets the value of an index. The index should be less than the arrays
size, otherwise this errors.
-}
set :: (HasCallStack, Prim a) => Int -> a -> PrimArray a %1 -> PrimArray a
set i x arr = unsafeSet i x (assertIndexInRange i arr)

-- | Same as 'set', but takes the 'PrimArray' as the first parameter.
write :: (HasCallStack, Prim a) => PrimArray a %1 -> Int -> a -> PrimArray a
write arr i a = set i a arr

{- | Same as 'set', but does not do bounds-checking. The behaviour is undefined
if an out-of-bounds index is provided.
-}
unsafeSet :: (Prim a) => Int -> a -> PrimArray a %1 -> PrimArray a
unsafeSet ix val (PrimArray arr) =
  PrimArray (Unlifted.set ix val arr)

fill :: (Prim a) => a -> PrimArray a %1 -> PrimArray a
fill a (PrimArray arr) = PrimArray (Unlifted.fill a arr)

-- | Same as 'unsafeSet', but takes the 'PrimArray' as the first parameter.
unsafeWrite :: (Prim a) => PrimArray a %1 -> Int -> a -> PrimArray a
unsafeWrite arr i a = unsafeSet i a arr

{- | Get the value of an index. The index should be less than the arrays 'size',
otherwise this errors.
-}
get :: (HasCallStack, Prim a) => Int -> PrimArray a %1 -> (Ur a, PrimArray a)
get i arr = unsafeGet i (assertIndexInRange i arr)

-- | Same as 'get', but takes the 'PrimArray' as the first parameter.
read :: (HasCallStack, Prim a) => PrimArray a %1 -> Int -> (Ur a, PrimArray a)
read arr i = get i arr

{- | Same as 'get', but does not do bounds-checking. The behaviour is undefined
if an out-of-bounds index is provided.
-}
unsafeGet :: (Prim a) => Int -> PrimArray a %1 -> (Ur a, PrimArray a)
unsafeGet ix (PrimArray arr) = wrap (Unlifted.get ix arr)
  where
    wrap :: (# Ur a, PrimArray# a #) %1 -> (Ur a, PrimArray a)
    wrap (# ret, arr #) = (ret, PrimArray arr)

-- | Same as 'unsafeGet', but takes the 'PrimArray' as the first parameter.
unsafeRead :: (Prim a) => PrimArray a %1 -> Int -> (Ur a, PrimArray a)
unsafeRead arr i = unsafeGet i arr

{- | Resize an array. That is, given an array, a target size, and a seed
value; resize the array to the given size using the seed value to fill
in the new cells when necessary and copying over all the unchanged cells.

Target size should be non-negative.

@
let b = resize n x a,
  then size b = n,
  and b[i] = a[i] for i < size a,
  and b[i] = x for size a <= i < n.
@
-}
resize :: (HasCallStack, Prim a) => Int -> a -> PrimArray a %1 -> PrimArray a
resize newSize seed (PrimArray arr :: PrimArray a)
  | newSize < 0 =
      Unlifted.lseq
        arr
        (error "Trying to resize to a negative size.")
  | otherwise =
      unsafeResize newSize (Just seed) (PrimArray arr)

unsafeResize :: (Prim a) => Int -> Maybe a -> PrimArray a %1 -> PrimArray a
unsafeResize newSize seed (PrimArray arr :: PrimArray a) =
  case seed of
    Nothing -> doCopy (Unlifted.unsafeAllocBeside newSize arr)
    Just seed -> doCopy (Unlifted.allocBeside newSize seed arr)
  where
    doCopy :: (# PrimArray# a, PrimArray# a #) %1 -> PrimArray a
    doCopy (# new, old #) = wrap (Unlifted.copyInto 0 old new)

    wrap :: (# PrimArray# a, PrimArray# a #) %1 -> PrimArray a
    wrap (# src, dst #) = src `Unlifted.lseq` PrimArray dst

-- | Return the array elements as a lazy list.
toList :: (Prim a) => PrimArray a %1 -> Ur [a]
toList (PrimArray arr) = Unlifted.toList arr

{- | Copy a slice of the array, starting from given offset and copying given
number of elements. Returns the pair (oldArray, slice).

Start offset + target size should be within the input array, and both should
be non-negative.

@
let b = slice i n a,
  then size b = n,
  and b[j] = a[i+j] for 0 <= j < n
@
-}
slice ::
  (HasCallStack, Prim a) =>
  -- | Start offset
  Int ->
  -- | Target size
  Int ->
  PrimArray a %1 ->
  (PrimArray a, PrimArray a)
slice from targetSize (arr :: PrimArray a) =
  size arr & \case
    (Ur s, PrimArray old)
      | s < from + targetSize ->
          Unlifted.lseq
            old
            (error "Slice index out of bounds.")
      | otherwise -> unsafeSlice from targetSize (PrimArray old)

unsafeSlice ::
  (Prim a) =>
  -- | Start offset
  Int ->
  -- | Target size
  Int ->
  PrimArray a %1 ->
  (PrimArray a, PrimArray a)
unsafeSlice from targetSize (PrimArray old :: PrimArray a) =
  doCopy
    ( Unlifted.allocBeside
        targetSize
        (error "invariant violation: uninitialized array index")
        old
    )
  where
    doCopy :: (# PrimArray# a, PrimArray# a #) %1 -> (PrimArray a, PrimArray a)
    doCopy (# new, old #) = wrap (Unlifted.copyInto from old new)

    wrap :: (# PrimArray# a, PrimArray# a #) %1 -> (PrimArray a, PrimArray a)
    wrap (# old, new #) = (PrimArray old, PrimArray new)

{- | /O(1)/ Convert an 'PrimArray' to an immutable primitive 'PV.Vector' (from
'vector' package).
-}
freeze :: (Prim a) => PrimArray a %1 -> Ur (PV.Vector a)
freeze (PrimArray arr) =
  Unlifted.freeze
    (\pa@(Prim.PrimArray a) -> PV.Vector 0 (Prim.sizeofPrimArray pa) (Prim.ByteArray a))
    arr

map :: (Prim a, Prim b) => (a -> b) -> PrimArray a %1 -> PrimArray b
map f (PrimArray arr) = PrimArray (Unlifted.map f arr)

instance (Prim a) => C.Array PrimArray a where
  size = size
  fromList = fromList
  unsafeAlloc = unsafeAlloc
  unsafeAllocL = unsafeAllocL
  fill = fill
  unsafeSet = unsafeSet
  unsafeGet = unsafeGet
  unsafeSlice = unsafeSlice
  unsafeResize i = unsafeResize i Nothing
