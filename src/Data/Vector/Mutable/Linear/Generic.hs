{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

module Data.Vector.Mutable.Linear.Generic (
  Vector (),
  empty,
  emptyL,
  constant,
  constantL,
  fromArray,
  fromList,
  fromListL,
  size,
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
  mapMaybeSame,
  filter,
  slice,
  toList,
) where

import qualified Control.Functor.Linear as C
import Control.Monad (guard)
import Data.Alloc.Linearly.Token
import Data.Alloc.Linearly.Token.Unsafe (HasLinearWitness)
import Data.Array.Mutable.Linear.Class (Array)
import qualified Data.Array.Mutable.Linear.Class as Array
import qualified Data.Bifunctor.Linear as BiL
import qualified Data.Unrestricted.Linear as Ur
import GHC.Stack (HasCallStack)
import Prelude.Linear hiding (filter, mapMaybe)
import qualified Prelude as P

data Vector arr a where
  Vec :: {-# UNPACK #-} !Int -> arr a %1 -> Vector arr a
  deriving anyclass (HasLinearWitness)

instance Array arr a => Consumable (Vector arr a) where
  consume (Vec n arr) = n `lseq` consume arr

instance Array arr a => Dupable (Vector arr a) where
  dup2 (Vec n arr) =
    BiL.bimap (Vec n) (Vec n) (dup2 arr)

empty :: Array.Array arr a => (Vector arr a %1 -> Ur b) %1 -> Ur b
empty f = Array.unsafeAlloc 0 (f . Vec 0)

emptyL :: Array.Array arr a => Linearly %1 -> Vector arr a
emptyL l = Vec 0 $ Array.unsafeAllocL l 0

constant :: (HasCallStack, Array arr a) => Int -> a -> (Vector arr a %1 -> Ur b) %1 -> Ur b
constant n a f
  | n < 0 = error ("constant: must be non-negative but got: " <> show n) f
  | otherwise = Array.unsafeAlloc n (f . Vec n . Array.fill a)

constantL :: (HasCallStack, Array arr a) => Linearly %1 -> Int -> a -> Vector arr a
constantL l n a
  | n < 0 = error ("constant: must be non-negative but got: " <> show n) l
  | otherwise = Vec n (Array.fill a (Array.unsafeAllocL l n))

-- | Allocator from a list
fromList :: (Array arr a) => [a] -> (Vector arr a %1 -> Ur b) %1 -> Ur b
fromList xs f = Array.fromList xs (f . fromArray)

-- | Allocator from a list
fromListL :: (Array arr a) => Linearly %1 -> [a] -> Vector arr a
fromListL l xs = fromArray (Array.fromListL l xs)

fromArray :: Array arr a => arr a %1 -> Vector arr a
fromArray arr =
  Array.size arr & \(Ur n, arr) ->
    Vec n arr

size :: Vector arr a %1 -> (Ur Int, Vector arr a)
size (Vec n arr) = (Ur n, Vec n arr)

set :: (HasCallStack, Array arr a) => Int -> a -> Vector arr a %1 -> Vector arr a
set i a (Vec n arr)
  | i < n = unsafeSet i a (Vec n arr)
  | otherwise = arr `lseq` error ("set: Index out of bounds: " <> show (i, n))

unsafeSet :: Array arr a => Int -> a -> Vector arr a %1 -> Vector arr a
unsafeSet i x (Vec n arr) =
  Array.unsafeSet i x arr & \arr ->
    Vec n arr

modify :: (HasCallStack, Array arr a) => (a -> (a, b)) -> Int -> Vector arr a %1 -> (Ur b, Vector arr a)
modify f i (Vec n arr)
  | i < n = unsafeModify f i (Vec n arr)
  | otherwise = arr `lseq` error ("modify: out of bound: " <> show (i, n))

modify_ :: (HasCallStack, Array arr a) => (a -> a) -> Int -> Vector arr a %1 -> Vector arr a
modify_ f i vec =
  modify (\a -> (f a, ())) i vec & \(Ur (), vec) ->
    vec

unsafeModify :: Array arr a => (a -> (a, b)) -> Int -> Vector arr a %1 -> (Ur b, Vector arr a)
unsafeModify f i (Vec n arr) =
  Array.unsafeGet i arr & \(Ur a, arr) ->
    case f a of
      (a, b) ->
        Array.unsafeSet i a arr & \arr ->
          (Ur b, Vec n arr)

capacity :: Array arr a => Vector arr a %1 -> (Ur Int, Vector arr a)
capacity (Vec n arr) =
  Array.size arr & \(Ur cap, arr) ->
    (Ur cap, Vec n arr)

get :: (HasCallStack, Array arr a) => Int -> Vector arr a %1 -> (Ur a, Vector arr a)
get i (Vec n arr)
  | i < n = arr `lseq` error ("get: out of bound: " <> show (i, n))
  | otherwise = unsafeGet i (Vec n arr)

unsafeGet :: Array arr a => Int -> Vector arr a %1 -> (Ur a, Vector arr a)
unsafeGet i (Vec n arr) = C.fmap (Vec n) (Array.unsafeGet i arr)

{- | Resize the vector to not have any wasted memory (size == capacity). This
returns a semantically identical vector.
-}
shrinkToFit :: Array arr a => Vector arr a %1 -> Vector arr a
shrinkToFit vec =
  capacity vec & \(Ur cap, vec') ->
    size vec' & \(Ur s', vec'') ->
      if cap > s'
        then unsafeResize s' vec''
        else vec''

{- | Insert at the end of the vector. This will grow the vector if there
is no empty space.
-}
push :: Array arr a => a -> Vector arr a %1 -> Vector arr a
push x vec =
  growToFit 1 vec & \(Vec s arr) ->
    unsafeSet s x (Vec (s + 1) arr)

{- | Pop from the end of the vector. This will never shrink the vector, use
'shrinkToFit' to remove the wasted space.
-}
pop :: Array arr a => Vector arr a %1 -> (Ur (Maybe a), Vector arr a)
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
growToFit :: (Array arr a) => Int -> Vector arr a %1 -> Vector arr a
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
unsafeResize :: (Array arr a) => Int -> Vector arr a %1 -> Vector arr a
unsafeResize newSize (Vec size' ma) =
  Vec (min size' newSize) (Array.unsafeResize newSize ma)

-- | Filters the vector in-place. It does not deallocate unused capacity, use 'shrinkToFit' for that if necessary.
filter :: (Array arr a) => Vector arr a %1 -> (a -> Bool) -> Vector arr a
filter vec p = mapMaybeSame vec \x -> x P.<$ guard (p x)

-- | Analogous to 'mapMaybe', but has the same type.
mapMaybeSame :: (Array arr a) => Vector arr a %1 -> (a -> Maybe a) -> Vector arr a
mapMaybeSame (vec :: Vector arr a) (p :: a -> Maybe a) =
  size vec & \(Ur s, vec') -> go 0 0 s vec'
  where
    go ::
      Int -> -- read cursor
      Int -> -- write cursor
      Int -> -- input size
      Vector arr a %1 ->
      Vector arr a
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
            case p a of
              Just a' -> go (r + 1) (w + 1) s (unsafeSet w a' vec'')
              Nothing -> go (r + 1) w s vec''

{- | A version of 'fmap' which can throw out elements.

NOTE: Contrary to the Boxed case, we MUST NOT reuse original array
because the sizes / alignments can differ across element types.
-}
mapMaybe ::
  (Array arr a, Array arr b) =>
  Vector arr a %1 ->
  (a -> Maybe b) ->
  Vector arr b
{-# INLINE [1] mapMaybe #-}
mapMaybe (src :: Vector arr a) (f :: a -> Maybe b) =
  size src & \(Ur s, Vec n src) ->
    Array.unsafeAllocBeside n src & \(dst, src) ->
      go 0 0 s src dst
  where
    go ::
      Int -> -- read cursor
      Int -> -- write count
      Int -> -- total input number
      arr a %1 ->
      arr b %1 ->
      Vector arr b
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

{-# RULES "mapMaybe/Same" mapMaybe = mapMaybeSame #-}

slice :: (HasCallStack, Array arr a) => Int -> Int -> Vector arr a %1 -> Vector arr a
slice from newSize (Vec oldSize arr)
  | oldSize < from + newSize =
      arr `lseq` error "Slice index out of bounds"
  | from == 0 =
      Vec newSize arr
  | otherwise =
      Array.unsafeSlice from newSize arr & \(oldArr, newArr) ->
        oldArr `lseq` fromArray newArr

instance Array arr a => P.Semigroup (Vector arr a) where
  l <> r = l Prelude.Linear.<> r

instance Array arr a => Semigroup (Vector arr a) where
  v1 <> Vec n src = growToFit n v1 & \dst -> go 0 n src dst
    where
      go :: Int -> Int -> arr a %1 -> Vector arr a %1 -> Vector arr a
      go !i !n src (Vec sz dst)
        | i == n = src `lseq` Vec sz dst
        | otherwise =
            Array.unsafeGet i src & \(Ur a, src) ->
              Array.unsafeSet sz a dst & \dst ->
                go (i + 1) n src (Vec (sz + 1) dst)

-- | Return the vector elements as a lazy list.
toList :: Array arr a => Vector arr a %1 -> Ur [a]
toList (Vec n (arr :: arr a)) = go 0 n arr
  where
    go :: Int -> Int -> arr a %1 -> Ur [a]
    go !i !n arr
      | i == n = arr `lseq` Ur []
      | otherwise =
          Array.unsafeGet i arr & \(Ur x, arr) ->
            Ur.lift (x :) (go (i + 1) n arr)
