{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

{- HLINT ignore "Use const" -}

module Data.Array.Mutable.Linear.Storable.Borrowable (
  SArray (),
  Storable,
  allocL,
  unsafeAllocL,
  fromListL,
  fromVectorL,
  freeze,
  free,
  R,
  W,
  RW (..),
  NewArray (..),
  size,
  get,
  unsafeGet,
  set,
  unsafeSet,
  swap,
  unsafeSwap,
  SlicesTo (),
  Slice (..),
  split,
  halve,
  unsafeSplit,
  combine,
) where

import Control.Monad (unless)
import Data.Function (fix)
import Data.Kind (Type)
import qualified Data.Vector.Storable as SV
import Foreign (finalizerFree)
import qualified Foreign as F
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, peekElemOff, pokeElemOff)
import qualified GHC.Exts as GHC
import qualified GHC.IO as GHC
import qualified GHC.IO as IO
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Linear.Token.Linearly
import Linear.Token.Linearly.Unsafe (HasLinearWitness)
import Prelude.Linear
import Prelude.Linear.Unsatisfiable (Unsatisfiable, unsatisfiable)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

{- HLINT ignore MkNewArray "Redundant bracket" -}
type NewArray :: Type -> Type
data NewArray a where
  MkNewArray :: SArray a s -> RW s %1 -> NewArray a

data SArray a s where
  SArray :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !(Ptr a) -> SArray a s

type ZeroBitType = GHC.TYPE ('GHC.TupleRep '[])

type R :: forall {s}. s -> Type
data R s = R deriving anyclass (HasLinearWitness)

instance
  (Unsatisfiable ('ShowType (R s) ':<>: 'Text " is not Consumable by design")) =>
  Consumable (R s)
  where
  consume = unsatisfiable
  {-# INLINE consume #-}

type W :: forall {s}. s -> Type
data W s = W deriving anyclass (HasLinearWitness)

instance
  (Unsatisfiable ('ShowType (W s) ':<>: 'Text " is not Consumable by design")) =>
  Consumable (W s)
  where
  consume = unsatisfiable
  {-# INLINE consume #-}

type RW :: forall {s}. s -> Type
data RW s where
  RW :: !(R s) %1 -> !(W s) %1 -> RW s
  deriving anyclass (HasLinearWitness)

instance
  (Unsatisfiable ('ShowType (RW s) ':<>: 'Text " is not Consumable by design")) =>
  Consumable (RW s)
  where
  consume = unsatisfiable
  {-# INLINE consume #-}

unsafeAllocL ::
  (SV.Storable a) =>
  Int ->
  Linearly %1 ->
  NewArray a
{-# NOINLINE unsafeAllocL #-}
unsafeAllocL n l =
  l `lseq` withUnsafeStrictPerformIO (mallocArray n) \ptr ->
    SArray n ptr `MkNewArray` RW R W

allocL ::
  (HasCallStack, SV.Storable a) =>
  Int ->
  a ->
  Linearly %1 ->
  NewArray a
{-# NOINLINE allocL #-}
allocL n a l
  | n < 0 = l `lseq` error ("allocL: negative length: " <> show n)
  | otherwise =
      l `lseq` withUnsafeStrictPerformIO (mallocArray n) \ptr ->
        fix
          ( \self !i ->
              unless (i == n) $
                pokeElemOff ptr i a P.>> self (i + 1)
          )
          0
          `withUnsafeStrictPerformIO_` (SArray n ptr `MkNewArray` RW R W)

fromListL :: (Storable a) => [a] -> Linearly %1 -> NewArray a
{-# NOINLINE fromListL #-}
fromListL (xs :: [a]) l =
  l `lseq`
    let len = P.length xs
     in withUnsafeStrictPerformIO (newArray xs) $ \ptr ->
          SArray len ptr `MkNewArray` RW R W

fromVectorL :: (Storable a) => SV.Vector a -> Linearly %1 -> NewArray a
{-# NOINLINE fromVectorL #-}
fromVectorL xs l =
  unsafeAllocL (SV.length xs) l & \(SArray sz ptr `MkNewArray` RW R W) ->
    SV.unsafeWith xs (\src -> copyArray ptr src sz)
      `withUnsafeStrictPerformIO_` (SArray sz ptr `MkNewArray` RW R W)

withUnsafeStrictPerformIO_ :: IO () -> a %1 -> a
{-# INLINE withUnsafeStrictPerformIO_ #-}
withUnsafeStrictPerformIO_ act = Unsafe.toLinear \x ->
  case GHC.runRW# $ GHC.unIO (do do { () <- act; P.pure x }) of
    (# _, !a #) -> GHC.lazy a

unsafeStrictPerformIO :: IO a %1 -> a
{-# INLINE unsafeStrictPerformIO #-}
unsafeStrictPerformIO = Unsafe.toLinear \act ->
  case GHC.runRW# $ GHC.unIO do IO.evaluate P.=<< act of
    (# _, !a #) -> GHC.lazy a

withUnsafeStrictPerformIO :: IO a %1 -> (a -> b) %1 -> b
{-# INLINE withUnsafeStrictPerformIO #-}
withUnsafeStrictPerformIO = Unsafe.toLinear2 \act f ->
  case GHC.runRW# $ GHC.unIO do !a <- act; IO.evaluate (f a) of
    (# _, b #) -> GHC.lazy b

freeze :: forall a s. (SV.Storable a) => RW s %1 -> SArray a s -> Ur (SV.Vector a)
{-# NOINLINE freeze #-}
freeze = GHC.noinline $ \(RW R W) (SArray l ptr) ->
  newForeignPtr finalizerFree ptr
    `withUnsafeStrictPerformIO` \fptr -> Ur (SV.unsafeFromForeignPtr0 fptr l)

free :: RW s %1 -> SArray a s -> ()
{-# NOINLINE free #-}
free (RW R W) (SArray _ mu) = unsafeStrictPerformIO (F.free mu)

size :: R s %1 -> SArray a s -> (Ur Int, R s)
{-# NOINLINE size #-}
size !r (SArray len _) = (Ur len, r)

get :: (SV.Storable a, HasCallStack) => R s %1 -> Int -> SArray a s -> (Ur a, R s)
{-# INLINE get #-}
get r i arr@(SArray sz _) =
  if 0 <= i && i < sz
    then unsafeGet r i arr
    else error ("get: out of bounds: " <> show (i, sz)) r

unsafeGet :: (SV.Storable a) => R s %1 -> Int -> SArray a s -> (Ur a, R s)
{-# NOINLINE unsafeGet #-}
unsafeGet = GHC.noinline \ !r i (SArray _ ptr) ->
  (Ur P.<$> peekElemOff ptr i) `withUnsafeStrictPerformIO` (,r)

set :: (SV.Storable a, HasCallStack) => RW s %1 -> Int -> a -> SArray a s -> RW s
{-# INLINE set #-}
set !rw i a arr@(SArray sz _) =
  if 0 <= i && i < sz
    then unsafeSet rw i a arr
    else error ("set: out of bounds: " <> show (i, sz)) rw

unsafeSet :: (SV.Storable a) => RW s %1 -> Int -> a -> SArray a s -> RW s
{-# NOINLINE unsafeSet #-}
unsafeSet (RW r w) !i !a (SArray _ !ptr) =
  do { GHC.noDuplicate; pokeElemOff ptr i a } `withUnsafeStrictPerformIO_` RW r w

type SlicesTo :: forall {s}. s -> s -> s -> ZeroBitType
newtype SlicesTo s l r = SlicesTo_ GHC.Void#

pattern SlicesTo :: SlicesTo s l r
pattern SlicesTo <- SlicesTo_ _ where SlicesTo = SlicesTo_ GHC.void#

data Slice a s where
  MkSlice ::
    !(SlicesTo s l r) %1 ->
    !(RW l) %1 ->
    !(RW r) %1 ->
    !(SArray a l) ->
    !(SArray a r) ->
    Slice a s

unsafeSplit :: (SV.Storable a) => RW s %1 -> Int -> SArray a s -> Slice a s
{-# NOINLINE unsafeSplit #-}
unsafeSplit = GHC.noinline \(RW R W) lenL (SArray len v) ->
  let !lenR = len - lenL
   in MkSlice
        SlicesTo
        (RW R W)
        (RW R W)
        (SArray lenL v)
        (SArray lenR (v `advancePtr` lenL))

split :: (SV.Storable a, HasCallStack) => RW s %1 -> Int -> SArray a s -> Slice a s
{-# INLINE split #-}
split !rw l arr@(SArray n _) =
  if 0 <= l && l < n
    then unsafeSplit rw l arr
    else error ("split: Index out of bounds: " <> show (l, n)) rw

combine ::
  SlicesTo s l r %1 ->
  RW l %1 ->
  RW r %1 ->
  SArray a l ->
  SArray a r ->
  (Ur (SArray a s), RW s)
{-# NOINLINE combine #-}
combine
  SlicesTo_ {}
  !rw
  (RW R W)
  (SArray lenL v)
  (SArray lenR _) =
    (Ur (SArray (lenL + lenR) v), Unsafe.coerce rw)

halve :: (SV.Storable a) => RW s %1 -> SArray a s -> Slice a s
{-# INLINE halve #-}
halve !rw arr@(SArray ln _) = unsafeSplit rw (ln `quot` 2) arr

unsafeSwap :: (SV.Storable a) => RW s %1 -> Int -> Int -> SArray a s -> RW s
{-# INLINE unsafeSwap #-}
unsafeSwap (RW r w) i j sa =
  unsafeGet r i sa & \(Ur ai, !r) ->
    unsafeGet r j sa & \(Ur aj, !r) ->
      unsafeSet (RW r w) i aj sa & \ !rw ->
        unsafeSet rw j ai sa

swap :: (SV.Storable a, HasCallStack) => RW s %1 -> Int -> Int -> SArray a s -> RW s
{-# INLINE swap #-}
swap !rw i j sa@(SArray len _) =
  if 0 <= i && i < len && 0 <= j && j < len
    then unsafeSwap rw i j sa
    else error ("swap: out of bounds: " <> show (len, (i, j))) rw
