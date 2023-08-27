{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

module Data.Vector.Mutable.Linear.Unboxed (
  Vector (),
  empty,
  emptyL,
  constant,
  constantL,
  fromArray,
  fromList,
  fromListL,
  fromVectorL,
  size,
  slice',
  capacity,
  set,
  unsafeSet,
  modify,
  modify_,
  unsafeModify,
  get,
  unsafeGet,
  push,
  pop,
  shrinkToFit,
  mapMaybe,
  map,
  mapSame,
  filter,
  slice,
  toList,
  freeze,
  appendVector,
) where

import qualified Control.Functor.Linear as C
import Data.Alloc.Linearly.Token
import Data.Alloc.Linearly.Token.Unsafe (HasLinearWitness)
import Data.Array.Mutable.Linear.Unboxed (UArray)
import qualified Data.Array.Mutable.Linear.Unboxed as Array
import qualified Data.Array.Mutable.Linear.Unboxed.Internal as Array
import qualified Data.Bifunctor.Linear as BiL
import qualified Data.Unrestricted.Linear as Ur
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Exts (runRW#)
import GHC.IO (unIO)
import GHC.Stack (HasCallStack)
import Prelude.Linear hiding (filter, map, mapMaybe)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

data Vector a where
  Vec :: {-# UNPACK #-} !Int -> UArray a %1 -> Vector a
  deriving anyclass (HasLinearWitness)

instance Consumable (Vector a) where
  consume (Vec n arr) = n `lseq` consume arr

instance U.Unbox a => Dupable (Vector a) where
  dup2 (Vec n arr) =
    BiL.bimap (Vec n) (Vec n) (dup2 arr)

empty :: U.Unbox a => (Vector a %1 -> Ur b) %1 -> Ur b
empty f = Array.unsafeAlloc 0 (f . Vec 0)

emptyL :: U.Unbox a => Linearly %1 -> Vector a
emptyL l = Vec 0 $ Array.unsafeAllocL l 0

constant :: (HasCallStack, U.Unbox a) => Int -> a -> (Vector a %1 -> Ur b) %1 -> Ur b
constant n a f
  | n < 0 = error ("constant: must be non-negative but got: " <> show n) f
  | otherwise = Array.unsafeAlloc n (f . Vec n . Array.fill a)

constantL :: (HasCallStack, U.Unbox a) => Linearly %1 -> Int -> a -> Vector a
constantL l n a
  | n < 0 = error ("constant: must be non-negative but got: " <> show n) l
  | otherwise = Vec n (Array.fill a (Array.unsafeAllocL l n))

-- | Allocator from a list
fromList :: (U.Unbox a) => [a] -> (Vector a %1 -> Ur b) %1 -> Ur b
fromList xs f = Array.fromList xs (f . fromArray)

-- | Allocator from a list
fromListL :: (U.Unbox a) => Linearly %1 -> [a] -> Vector a
fromListL l xs = fromArray $ Array.fromListL l xs

fromVectorL :: (U.Unbox a) => Linearly %1 -> U.Vector a %1 -> Vector a
fromVectorL l = fromArray . Array.fromVectorL l

fromArray :: U.Unbox a => UArray a %1 -> Vector a
fromArray arr =
  Array.size arr & \(Ur n, arr) ->
    Vec n arr

size :: Vector a %1 -> (Ur Int, Vector a)
size (Vec n arr) = (Ur n, Vec n arr)

set :: (HasCallStack, U.Unbox a) => Int -> a -> Vector a %1 -> Vector a
set i a (Vec n arr)
  | i < n = unsafeSet i a (Vec n arr)
  | otherwise = arr `lseq` error ("set: Index out of bounds: " <> show (i, n))

unsafeSet :: U.Unbox a => Int -> a -> Vector a %1 -> Vector a
unsafeSet i x (Vec n arr) =
  Array.unsafeSet i x arr & \arr ->
    Vec n arr

modify :: (HasCallStack, U.Unbox a) => (a -> (a, b)) -> Int -> Vector a %1 -> (Ur b, Vector a)
modify f i (Vec n arr)
  | i < n = unsafeModify f i (Vec n arr)
  | otherwise = arr `lseq` error ("modify: out of bound: " <> show (i, n))

modify_ :: (HasCallStack, U.Unbox a) => (a -> a) -> Int -> Vector a %1 -> Vector a
modify_ f i vec =
  modify (\a -> (f a, ())) i vec & \(Ur (), vec) ->
    vec

unsafeModify :: U.Unbox a => (a -> (a, b)) -> Int -> Vector a %1 -> (Ur b, Vector a)
unsafeModify f i (Vec n arr) =
  Array.unsafeGet i arr & \(Ur a, arr) ->
    case f a of
      (a, b) ->
        Array.unsafeSet i a arr & \arr ->
          (Ur b, Vec n arr)

capacity :: U.Unbox a => Vector a %1 -> (Ur Int, Vector a)
capacity (Vec n arr) =
  Array.size arr & \(Ur cap, arr) ->
    (Ur cap, Vec n arr)

get :: (HasCallStack, U.Unbox a) => Int -> Vector a %1 -> (Ur a, Vector a)
get i (Vec n arr)
  | i < n = unsafeGet i (Vec n arr)
  | otherwise = arr `lseq` error ("get: out of bound: " <> show (i, n))

unsafeGet :: U.Unbox a => Int -> Vector a %1 -> (Ur a, Vector a)
unsafeGet i (Vec n arr) = C.fmap (Vec n) (Array.unsafeGet i arr)

{- | Resize the vector to not have any wasted memory (size == capacity). This
returns a semantically identical vector.
-}
shrinkToFit :: U.Unbox a => Vector a %1 -> Vector a
shrinkToFit vec =
  capacity vec & \(Ur cap, vec') ->
    size vec' & \(Ur s', vec'') ->
      if cap > s'
        then unsafeResize s' vec''
        else vec''

{- | Insert at the end of the vector. This will grow the vector if there
is no empty space.
-}
push :: U.Unbox a => a -> Vector a %1 -> Vector a
push x vec =
  growToFit 1 vec & \(Vec s arr) ->
    unsafeSet s x (Vec (s + 1) arr)

{- | Push a raw unboxed 'U.Vector' from @vector@ package at the end.
This always copies the pushed vector.
-}
appendVector :: U.Unbox a => U.Vector a -> Vector a %1 -> Vector a
{-# NOINLINE appendVector #-}
appendVector news vec =
  let newLen = U.length news
   in growToFit newLen vec & Unsafe.toLinear \(Vec s (Array.UArray arr)) ->
        case runRW# $
          unIO $
            U.unsafeCopy
              (MU.unsafeSlice (s - newLen) newLen arr)
              news of
          (# _, () #) -> Vec s (Array.UArray arr)

{- | Pop from the end of the vector. This will never shrink the vector, use
'shrinkToFit' to remove the wasted space.
-}
pop :: U.Unbox a => Vector a %1 -> (Ur (Maybe a), Vector a)
pop vec =
  size vec & \case
    (Ur 0, vec') ->
      (Ur Nothing, vec')
    (Ur s, vec') ->
      unsafeGet (s - 1) vec' & \(Ur a, Vec _ arr) ->
        ( Ur (Just a)
        , Vec (s - 1) arr
        )

constGrowthFactor :: Int
constGrowthFactor = 2

{- | Grows the vector to the closest power of growthFactor to
fit at least n more elements.
-}
growToFit :: (U.Unbox a) => Int -> Vector a %1 -> Vector a
growToFit n vec =
  capacity vec & \(Ur cap, vec') ->
    size vec' & \(Ur s', vec'') ->
      if s' + n <= cap
        then vec''
        else
          let
            -- Calculate the closest power of growth factor
            -- larger than required size.
            newSize =
              constGrowthFactor -- This constant is defined above.
                ^ (ceiling :: Double -> Int)
                  ( logBase
                      (fromIntegral constGrowthFactor)
                      (fromIntegral (s' + n)) -- this is always
                      -- > 0 because of
                      -- the if condition
                  )
           in
            unsafeResize
              newSize
              vec''

{- | Resize the vector to a non-negative size. In-range elements are preserved,
the possible new elements are bottoms.
-}
unsafeResize :: (U.Unbox a) => Int -> Vector a %1 -> Vector a
unsafeResize newSize (Vec size' ma) =
  Vec (min size' newSize) (Array.unsafeResize newSize ma)

-- | Filters the vector in-place. It does not deallocate unused capacity, use 'shrinkToFit' for that if necessary.
filter :: (U.Unbox a) => Vector a %1 -> (a -> Bool) -> Vector a
filter (vec :: Vector a) (p :: a -> Bool) =
  size vec & \(Ur s, vec') -> go 0 0 s vec'
  where
    go ::
      Int -> -- read cursor
      Int -> -- write cursor
      Int -> -- input size
      Vector a %1 ->
      Vector a
    go r w s vec'
      -- If we processed all elements, set the capacity after the last written
      -- index and coerce the result to the correct type.
      | r == s =
          vec' & \(Vec _ arr) ->
            Vec w arr
      -- Otherwise, read an element, write if the predicate is true and advance
      -- the write cursor; otherwise keep the write cursor skipping the element.
      | otherwise =
          unsafeGet r vec' & \(Ur a, vec'') ->
            if p a
              then go (r + 1) (w + 1) s (unsafeSet w a vec'')
              else go (r + 1) w s vec''

{- | A version of 'fmap' which can throw out elements.

NOTE: Contrary to the Boxed case, we MUST NOT reuse original array
because the sizes / alignments can differ across element types.
-}
mapMaybe ::
  (U.Unbox a, U.Unbox b) =>
  Vector a %1 ->
  (a -> Maybe b) ->
  Vector b
mapMaybe (src :: Vector a) (f :: a -> Maybe b) =
  size src & \(Ur s, Vec n src) ->
    Array.unsafeAllocBeside n src & \(dst, src) ->
      go 0 0 s src dst
  where
    go ::
      Int -> -- read cursor
      Int -> -- write count
      Int -> -- total input number
      UArray a %1 ->
      UArray b %1 ->
      Vector b
    go r w s src dst
      -- If we processed all elements, set the capacity after the last written
      -- index and coerce the result to the correct type.
      | r == s = src `lseq` Vec w dst
      -- Otherwise, read an element, write if the predicate is true and advance
      -- the write cursor; otherwise keep the write cursor skipping the element.
      | otherwise =
          Array.unsafeGet r src & \case
            (Ur a, src)
              | Just b <- f a ->
                  go (r + 1) (w + 1) s src (Array.unsafeSet w b dst)
              | otherwise ->
                  go (r + 1) w s src dst

{- | 'fmap', but unboxed.

NOTE: Contrary to the Boxed case, we MUST NOT reuse original array
because the sizes / alignments can differ across element types.
-}
map ::
  (U.Unbox a, U.Unbox b) =>
  Vector a %1 ->
  (a -> b) ->
  Vector b
{-# INLINE [1] map #-}
map (src :: Vector a) (f :: a -> b) =
  size src & \(Ur s, Vec n src) ->
    Array.unsafeAllocBeside n src & \(dst, src) ->
      go 0 s src dst
  where
    go ::
      Int -> -- cursor
      Int -> -- total input number
      UArray a %1 ->
      UArray b %1 ->
      Vector b
    go r s src dst
      -- If we processed all elements, set the capacity after the last written
      -- index and coerce the result to the correct type.
      | r == s = src `lseq` Vec r dst
      -- Otherwise, read an element, write if the predicate is true and advance
      -- the write cursor; otherwise keep the write cursor skipping the element.
      | otherwise =
          Array.unsafeGet r src & \(Ur a, src) ->
            go (r + 1) s src (Array.unsafeSet r (f a) dst)

{-# RULES "map/mapSame" map = mapSame #-}

-- | 'map' with the same argument and return type.
mapSame ::
  (U.Unbox a) =>
  Vector a %1 ->
  (a -> a) ->
  Vector a
mapSame (src :: Vector a) (f :: a -> b) =
  src & \(Vec n src) -> go 0 n src
  where
    go ::
      Int -> -- cursor
      Int -> -- total input number
      UArray a %1 ->
      Vector a
    go r s src
      -- If we processed all elements, set the capacity after the last written
      -- index and coerce the result to the correct type.
      | r == s = Vec r src
      -- Otherwise, read an element, write if the predicate is true and advance
      -- the write cursor; otherwise keep the write cursor skipping the element.
      | otherwise =
          Array.unsafeGet r src & \(Ur a, src) ->
            go (r + 1) s (Array.unsafeSet r (f a) src)

slice :: (HasCallStack, U.Unbox a) => Int -> Int -> Vector a %1 -> Vector a
slice from newSize (Vec oldSize arr)
  | oldSize < from + newSize =
      arr `lseq` error "Slice index out of bounds"
  | from == 0 =
      Vec newSize arr
  | otherwise =
      Array.unsafeSlice from newSize arr & \(oldArr, newArr) ->
        oldArr `lseq` fromArray newArr

-- | Return value is @(orig, slice)@
slice' :: (HasCallStack, U.Unbox a) => Int -> Int -> Vector a %1 -> (Vector a, Vector a)
{-# INLINE slice' #-}
{-# ANN slice' "HLint: ignore Use bimap" #-}
slice' from newSize (Vec oldSize arr)
  | oldSize < from + newSize =
      arr `lseq` error "Slice index out of bounds"
  | from == 0 =
      dup2 arr & \(arr, arr') ->
        (Vec newSize arr, Vec newSize arr')
  | otherwise =
      Array.unsafeSlice from newSize arr & \(oldArr, newArr) ->
        (Vec oldSize oldArr, fromArray newArr)

instance U.Unbox a => P.Semigroup (Vector a) where
  l <> r = l Prelude.Linear.<> r

instance U.Unbox a => Semigroup (Vector a) where
  v1 <> Vec n src = growToFit n v1 & \dst -> go 0 n src dst
    where
      go :: Int -> Int -> UArray a %1 -> Vector a %1 -> Vector a
      go !i !n src (Vec sz dst)
        | i == n = src `lseq` Vec sz dst
        | otherwise =
            Array.unsafeGet i src & \(Ur a, src) ->
              Array.unsafeSet sz a dst & \dst ->
                go (i + 1) n src (Vec (sz + 1) dst)

-- | Return the vector elements as a lazy list.
toList :: U.Unbox a => Vector a %1 -> Ur [a]
toList (Vec n (arr :: arr a)) = go 0 n arr
  where
    go :: Int -> Int -> arr a %1 -> Ur [a]
    go !i !n arr
      | i == n = arr `lseq` Ur []
      | otherwise =
          Array.unsafeGet i arr & \(Ur x, arr) ->
            Ur.lift (x :) (go (i + 1) n arr)

{- | /O(1)/ freeze. This will never shrink the vector, use
'shrinkToFit' before freezing or 'U.force' after 'freeez'ing to remove the wasted space.
-}
freeze :: U.Unbox a => Vector a %1 -> Ur (U.Vector a)
freeze (Vec n uv) = Ur.lift (U.unsafeTake n) $ Array.freeze uv
