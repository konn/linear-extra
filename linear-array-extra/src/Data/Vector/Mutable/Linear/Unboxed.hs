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
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
  slice,
  unsafeSlice,
  slice',
  unsafeSlice',
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
  imap,
  imapSame,
  filter,
  toList,
  freeze,
  appendVector,
  withSlice,
  withSliceM,
  withUnsafeSlice,
  withUnsafeSliceM,
  Slice (),
  getS,
  unsafeGetS,
  cloneS,
  sizeS,
  foldlSL',
  foldMapS',
  foldMapSL',
  foldS',
  ifoldS',
  foldSML',
  ifoldSML',
) where

import qualified Control.Functor.Linear as C
import Control.Monad.Fix (fix)
import Data.Array.Mutable.Linear.Unboxed (UArray)
import qualified Data.Array.Mutable.Linear.Unboxed as Array
import qualified Data.Array.Mutable.Linear.Unboxed.Internal as Array
import qualified Data.Bifunctor.Linear as BiL
import Data.Coerce (coerce)
import Data.Unrestricted.Linear (UrT (..), runUrT)
import qualified Data.Unrestricted.Linear as Ur
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Exts (runRW#)
import GHC.IO (unIO)
import GHC.Stack (HasCallStack)
import Linear.Witness.Token
import Linear.Witness.Token.Unsafe (HasLinearWitness)
import Prelude.Linear hiding (filter, map, mapMaybe)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

data Vector a where
  Vec :: {-# UNPACK #-} !Int -> UArray a %1 -> Vector a
  deriving anyclass (HasLinearWitness)

instance Consumable (Vector a) where
  consume (Vec n arr) = n `lseq` consume arr

instance (U.Unbox a) => Dupable (Vector a) where
  dup2 (Vec n arr) =
    BiL.bimap (Vec n) (Vec n) (dup2 arr)

empty :: (U.Unbox a) => (Vector a %1 -> Ur b) %1 -> Ur b
empty f = Array.unsafeAlloc 0 (f . Vec 0)

emptyL :: (U.Unbox a) => Linearly %1 -> Vector a
emptyL = Vec 0 . Array.unsafeAllocL 0

constant :: (HasCallStack, U.Unbox a) => Int -> a -> (Vector a %1 -> Ur b) %1 -> Ur b
constant n a f
  | n < 0 = error ("constant: must be non-negative but got: " <> show n) f
  | otherwise = Array.unsafeAlloc n (f . Vec n . Array.fill a)

constantL :: (HasCallStack, U.Unbox a) => Int -> a -> Linearly %1 -> Vector a
constantL n a
  | n < 0 = error $ "constant: must be non-negative but got: " <> show n
  | otherwise = Vec n . Array.fill a . Array.unsafeAllocL n

-- | Allocator from a list
fromList :: (U.Unbox a) => [a] -> (Vector a %1 -> Ur b) %1 -> Ur b
fromList xs f = Array.fromList xs (f . fromArray)

-- | Allocator from a list
fromListL :: (U.Unbox a) => [a] -> Linearly %1 -> Vector a
fromListL xs = fromArray . Array.fromListL xs

fromVectorL :: (U.Unbox a) => U.Vector a %1 -> Linearly %1 -> Vector a
fromVectorL xs = fromArray . Array.fromVectorL xs

fromArray :: (U.Unbox a) => UArray a %1 -> Vector a
fromArray arr =
  Array.size arr & \(Ur n, arr) ->
    Vec n arr

size :: Vector a %1 -> (Ur Int, Vector a)
size (Vec n arr) = (Ur n, Vec n arr)

set :: (HasCallStack, U.Unbox a) => Int -> a -> Vector a %1 -> Vector a
set i a (Vec n arr)
  | i < n = unsafeSet i a (Vec n arr)
  | otherwise = arr `lseq` error ("set: Index out of bounds: " <> show (i, n))

unsafeSet :: (U.Unbox a) => Int -> a -> Vector a %1 -> Vector a
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

unsafeModify :: (U.Unbox a) => (a -> (a, b)) -> Int -> Vector a %1 -> (Ur b, Vector a)
unsafeModify f i (Vec n arr) =
  Array.unsafeGet i arr & \(Ur a, arr) ->
    case f a of
      (a, b) ->
        Array.unsafeSet i a arr & \arr ->
          (Ur b, Vec n arr)

capacity :: (U.Unbox a) => Vector a %1 -> (Ur Int, Vector a)
capacity (Vec n arr) =
  Array.size arr & \(Ur cap, arr) ->
    (Ur cap, Vec n arr)

get :: (HasCallStack, U.Unbox a) => Int -> Vector a %1 -> (Ur a, Vector a)
get i (Vec n arr)
  | i < n = unsafeGet i (Vec n arr)
  | otherwise = arr `lseq` error ("get: out of bound: " <> show (i, n))

unsafeGet :: (U.Unbox a) => Int -> Vector a %1 -> (Ur a, Vector a)
unsafeGet i (Vec n arr) = C.fmap (Vec n) (Array.unsafeGet i arr)

{- | Resize the vector to not have any wasted memory (size == capacity). This
returns a semantically identical vector.
-}
shrinkToFit :: (U.Unbox a) => Vector a %1 -> Vector a
shrinkToFit vec =
  capacity vec & \(Ur cap, vec') ->
    size vec' & \(Ur s', vec'') ->
      if cap > s'
        then unsafeResize s' vec''
        else vec''

{- | Insert at the end of the vector. This will grow the vector if there
is no empty space.
-}
push :: (U.Unbox a) => a -> Vector a %1 -> Vector a
push x vec =
  growToFit 1 vec & \(Vec s arr) ->
    unsafeSet s x (Vec (s + 1) arr)

{- | Push a raw unboxed 'U.Vector' from @vector@ package at the end.
This always copies the pushed vector.
-}
appendVector :: (U.Unbox a) => U.Vector a -> Vector a %1 -> Vector a
{-# NOINLINE appendVector #-}
appendVector news vec =
  let growth = U.length news
   in growToFit growth vec & Unsafe.toLinear \(Vec s (Array.UArray arr)) ->
        case runRW# $
          unIO $
            U.unsafeCopy
              (MU.unsafeSlice s growth arr)
              news of
          (# _, () #) -> Vec (s + growth) (Array.UArray arr)

{- | Pop from the end of the vector. This will never shrink the vector, use
'shrinkToFit' to remove the wasted space.
-}
pop :: (U.Unbox a) => Vector a %1 -> (Ur (Maybe a), Vector a)
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
            unsafeResize newSize vec''

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

{- | 'fmapWithIndex', but unboxed.

NOTE: Contrary to the Boxed case, we MUST NOT reuse original array
because the sizes / alignments can differ across element types.
-}
imap ::
  (U.Unbox a, U.Unbox b) =>
  Vector a %1 ->
  (Int -> a -> b) ->
  Vector b
{-# INLINE [1] imap #-}
imap (src :: Vector a) (f :: Int -> a -> b) =
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
            go (r + 1) s src (Array.unsafeSet r (f r a) dst)

{-# RULES "imap/imapSame" imap = imapSame #-}

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

-- | 'map' with the same argument and return type.
imapSame ::
  (U.Unbox a) =>
  Vector a %1 ->
  (Int -> a -> a) ->
  Vector a
imapSame (src :: Vector a) (f :: Int -> a -> a) =
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
            go (r + 1) s (Array.unsafeSet r (f r a) src)

slice :: (HasCallStack, U.Unbox a) => Int -> Int -> Vector a %1 -> Vector a
slice from newSize (Vec oldSize arr)
  | oldSize < from + newSize =
      error ("Slice index out of bounds: (off, len, orig) = " <> show (from, newSize, oldSize)) arr
  | otherwise = unsafeSlice from newSize (Vec oldSize arr)

unsafeSlice :: (U.Unbox a) => Int -> Int -> Vector a %1 -> Vector a
{-# INLINE unsafeSlice #-}
unsafeSlice from newSize (Vec _ arr)
  | from == 0 = Vec newSize arr
  | otherwise =
      Array.unsafeSlice from newSize arr & \(oldArr, newArr) ->
        oldArr `lseq` fromArray newArr

-- | Return value is @(orig, slice)@
slice' :: (U.Unbox a) => Int -> Int -> Vector a %1 -> (Vector a, Vector a)
{-# INLINE slice' #-}
slice' from newSize (Vec oldSize arr)
  | oldSize < from + newSize =
      arr `lseq` error ("Slice index out of bounds: (off, len, orig) = " <> show (from, newSize, oldSize))
  | otherwise = unsafeSlice' from newSize (Vec oldSize arr)

unsafeSlice' :: (U.Unbox a) => Int -> Int -> Vector a %1 -> (Vector a, Vector a)
{-# INLINE unsafeSlice' #-}
{-# ANN unsafeSlice' "HLint: ignore Use bimap" #-}
unsafeSlice' from newSize (Vec oldSize arr)
  | from == 0 =
      dup2 arr & \(arr, arr') ->
        (Vec oldSize arr, Vec newSize arr')
  | otherwise =
      Array.unsafeSlice from newSize arr & \(oldArr, newArr) ->
        (Vec oldSize oldArr, fromArray newArr)

instance (U.Unbox a) => P.Semigroup (Vector a) where
  l <> r = l Prelude.Linear.<> r

instance (U.Unbox a) => Semigroup (Vector a) where
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
toList :: (U.Unbox a) => Vector a %1 -> Ur [a]
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
freeze :: (U.Unbox a) => Vector a %1 -> Ur (U.Vector a)
freeze (Vec n uv) = Ur.lift (U.unsafeTake n) $ Array.freeze uv

-- | Read-only slice of Unboxed vector
newtype Slice s a = Slice (UArray a)

withUnsafeSlice ::
  (U.Unbox a) =>
  Int ->
  Int ->
  (forall s. Slice s a %1 -> (b, Slice s a)) %1 ->
  Vector a %1 ->
  (b, Vector a)
{-# NOINLINE withUnsafeSlice #-}
withUnsafeSlice off ran f = Unsafe.toLinear \(Vec s (Array.UArray mu)) ->
  f (Slice (Array.UArray $ MU.unsafeSlice off ran mu)) & Unsafe.toLinear \(b, _) ->
    (b, Vec s (Array.UArray mu))

withUnsafeSliceM ::
  (C.Monad m, U.Unbox a) =>
  Int ->
  Int ->
  (forall s. Slice s a %1 -> m (b, Slice s a)) %1 ->
  Vector a %1 ->
  m (b, Vector a)
{-# NOINLINE withUnsafeSliceM #-}
withUnsafeSliceM off ran f = Unsafe.toLinear \(Vec s (Array.UArray mu)) ->
  f (Slice (Array.UArray $ MU.unsafeSlice off ran mu)) C.<&> Unsafe.toLinear \(b, _) ->
    (b, Vec s (Array.UArray mu))

withSlice ::
  (HasCallStack, U.Unbox a) =>
  Int ->
  Int ->
  (forall s. Slice s a %1 -> (b, Slice s a)) %1 ->
  Vector a %1 ->
  (b, Vector a)
{-# INLINE withSlice #-}
withSlice off newSize f (Vec oldSize arr)
  | oldSize < off + newSize =
      error ("Slice index out of bounds: (off, len, orig) = " <> show (off, newSize, oldSize)) f arr
  | otherwise = withUnsafeSlice off newSize f (Vec oldSize arr)

withSliceM ::
  (HasCallStack, C.Monad m, U.Unbox a) =>
  Int ->
  Int ->
  (forall s. Slice s a %1 -> m (b, Slice s a)) %1 ->
  Vector a %1 ->
  m (b, Vector a)
{-# INLINE withSliceM #-}
withSliceM off newSize f (Vec oldSize arr)
  | oldSize < off + newSize =
      error ("Slice index out of bounds: (off, len, orig) = " <> show (off, newSize, oldSize)) f arr
  | otherwise = withUnsafeSliceM off newSize f (Vec oldSize arr)

cloneS :: forall s a. (U.Unbox a) => Slice s a %1 -> (Slice s a, UArray a)
{-# INLINE cloneS #-}
cloneS = coerce $ dup2 @(UArray a)

unsafeGetS :: forall s a. (U.Unbox a) => Int -> Slice s a %1 -> (Ur a, Slice s a)
{-# INLINE unsafeGetS #-}
unsafeGetS = coerce $ Array.unsafeGet @a

getS :: forall s a. (HasCallStack, U.Unbox a) => Int -> Slice s a %1 -> (Ur a, Slice s a)
{-# INLINE getS #-}
getS = coerce $ Array.get @a

sizeS :: forall s a. (U.Unbox a) => Slice s a %1 -> (Ur Int, Slice s a)
{-# INLINE sizeS #-}
sizeS = coerce $ Array.size @a

foldlSL' :: (U.Unbox a) => (b %1 -> a -> b) -> b %1 -> Slice s a %1 -> (b, Slice s a)
{-# INLINE foldlSL' #-}
foldlSL' (f :: b %1 -> a -> b) b slc =
  sizeS slc & \(Ur n, slc) ->
    fix
      ( \go !i !b slc ->
          i == n & \case
            True -> (b, slc)
            False ->
              unsafeGetS i slc & \(Ur a, slc) ->
                f b a & \ !b ->
                  go (i + 1) b slc
      )
      0
      b
      slc

foldMapS' :: (U.Unbox a, P.Monoid w) => (a -> w) -> Slice s a %1 -> (Ur w, Slice s a)
{-# INLINE foldMapS' #-}
foldMapS' f arr =
  sizeS arr & \(Ur n, arr) ->
    fix
      ( \self !i !w !slc ->
          i == n & \case
            True -> (Ur w, slc)
            False ->
              unsafeGetS i slc & \(Ur a, slc) ->
                f a & \ !w' ->
                  (w P.<> w') & \ !w -> self (i + 1) w slc
      )
      0
      P.mempty
      arr

foldMapSL' :: (U.Unbox a, Monoid w) => (a -> w) -> Slice s a %1 -> (Ur w, Slice s a)
{-# INLINE foldMapSL' #-}
foldMapSL' f arr =
  sizeS arr & \(Ur n, arr) ->
    fix
      ( \self !i !w !slc ->
          i == n & \case
            True -> (Ur w, slc)
            False ->
              unsafeGetS i slc & \(Ur !a, slc) ->
                f a & \ !w' ->
                  (w <> w') & \ !w -> self (i + 1) w slc
      )
      0
      mempty
      arr

-- | For use with 'Control.Foldl.purely'.
foldS' :: (U.Unbox a) => (x -> a -> x) -> x -> (x -> b) -> Slice s a %1 -> (Ur b, Slice s a)
{-# INLINE foldS' #-}
{- HLINT ignore foldS' "Redundant lambda" -}
foldS' step ini out = \slc ->
  sizeS slc & \(Ur n, slc) ->
    fix
      ( \self !i !x !slc ->
          i == n & \case
            True -> (Ur (out x), slc)
            False ->
              unsafeGetS i slc & \(Ur !a, slc) ->
                step x a & \ !x ->
                  self (i + 1) x slc
      )
      0
      ini
      slc

-- | For use with 'Control.Foldl.purely'.
ifoldS' :: (U.Unbox a) => (x -> (Int, a) -> x) -> x -> (x -> b) -> Slice s a %1 -> (Ur b, Slice s a)
{-# INLINE ifoldS' #-}
{- HLINT ignore ifoldS' "Redundant lambda" -}
ifoldS' step ini out = \slc ->
  sizeS slc & \(Ur n, slc) ->
    fix
      ( \self !i !x !slc ->
          i == n & \case
            True -> (Ur (out x), slc)
            False ->
              unsafeGetS i slc & \(Ur !a, slc) ->
                step x (i, a) & \ !x ->
                  self (i + 1) x slc
      )
      0
      ini
      slc

-- | For use with 'Control.Foldl.impurely'.
foldSML' :: (U.Unbox a, C.Monad m) => (x -> a -> UrT m x) -> UrT m x -> (x -> UrT m b) -> Slice s a %1 -> m (Ur b, Slice s a)
{-# INLINE foldSML' #-}
{- HLINT ignore foldSML' "Redundant lambda" -}
foldSML' step ini out = \slc ->
  sizeS slc & \(Ur n, slc) -> C.do
    Ur !x0 <- runUrT ini
    fix
      ( \self !i !x !slc ->
          i == n & \case
            True -> (,slc) C.<$> runUrT (out x)
            False ->
              unsafeGetS i slc & \(Ur !a, slc) -> C.do
                Ur !x <- runUrT (step x a)
                self (i + 1) x slc
      )
      0
      x0
      slc

-- | For use with 'Control.Foldl.impurely'.
ifoldSML' :: (U.Unbox a, C.Monad m) => (x -> (Int, a) -> UrT m x) -> UrT m x -> (x -> UrT m b) -> Slice s a %1 -> m (Ur b, Slice s a)
{-# INLINE ifoldSML' #-}
{- HLINT ignore ifoldSML' "Redundant lambda" -}
ifoldSML' step ini out = \slc ->
  sizeS slc & \(Ur n, slc) -> C.do
    Ur !x0 <- runUrT ini
    fix
      ( \self !i !x !slc ->
          i == n & \case
            True -> (,slc) C.<$> runUrT (out x)
            False ->
              unsafeGetS i slc & \(Ur !a, slc) -> C.do
                Ur !x <- runUrT (step x (i, a))
                self (i + 1) x slc
      )
      0
      x0
      slc
