{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
  SuchThat (..),
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
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Generics.Linear.TH (deriveGeneric)
import Linear.Witness.Token
import Linear.Witness.Token.Unsafe (HasLinearWitness)
import Prelude.Linear
import Prelude.Linear.Unsatisfiable (Unsatisfiable, unsatisfiable)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

{- HLINT ignore SuchThat "Redundant bracket" -}
type SuchThat :: forall {s}. (s -> Type) -> (s -> Type) -> s -> Type
data SuchThat f g s where
  SuchThat :: f s -> g s %1 -> SuchThat f g s

data SArray a s where
  SArray :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !(Ptr a) %1 -> SArray a s

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

deriveGeneric ''RW

instance
  (Unsatisfiable ('ShowType (RW s) ':<>: 'Text " is not Consumable by design")) =>
  Consumable (RW s)
  where
  consume = unsatisfiable
  {-# INLINE consume #-}

{-
-- FIXME: more efficient implementation?
fill :: (Storable a) => a -> SArray a s %1 -> SArray a
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
   in unsafeStrictPerformIO go -}

unsafeAllocL ::
  (SV.Storable a) =>
  Int ->
  Linearly %1 ->
  SuchThat (SArray a) RW s
{-# NOINLINE unsafeAllocL #-}
unsafeAllocL n l =
  l `lseq` withUnsafeStrictPerformIO (mallocArray n) \ptr ->
    SArray n ptr `SuchThat` RW R W

allocL ::
  (HasCallStack, SV.Storable a) =>
  Int ->
  a ->
  Linearly %1 ->
  SuchThat (SArray a) RW s
{-# NOINLINE allocL #-}
allocL n a l
  | n < 0 = l `lseq` error ("allocL: negative length: " <> show n)
  | otherwise =
      l `lseq` withUnsafeStrictPerformIO (mallocArray n) \ptr ->
        withUnsafeStrictPerformIO
          ( fix
              ( \self !i ->
                  unless (i == n) $
                    pokeElemOff ptr i a P.>> self (i + 1)
              )
              0
          )
          \() -> SArray n ptr `SuchThat` RW R W

fromListL :: (Storable a) => [a] -> Linearly %1 -> SuchThat (SArray a) RW s
{-# NOINLINE fromListL #-}
fromListL (xs :: [a]) l =
  l `lseq`
    let len = P.length xs
     in withUnsafeStrictPerformIO (newArray xs) $ \ptr ->
          SArray len ptr `SuchThat` RW R W

fromVectorL :: (Storable a) => SV.Vector a -> Linearly %1 -> SuchThat (SArray a) RW s
{-# NOINLINE fromVectorL #-}
fromVectorL xs l =
  unsafeAllocL (SV.length xs) l & \(SArray sz ptr `SuchThat` RW R W) ->
    withUnsafeStrictPerformIO
      (SV.unsafeWith xs $ \src -> copyArray ptr src sz)
      \() -> SArray sz ptr `SuchThat` RW R W

unsafeStrictPerformIO :: IO a %1 -> a
{-# NOINLINE unsafeStrictPerformIO #-}
unsafeStrictPerformIO =
  GHC.noinline
    ( Unsafe.toLinear \act -> case GHC.runRW# (GHC.unIO act) of
        (# !_, !a #) -> a
    )

withUnsafeStrictPerformIO :: IO a %1 -> (a -> b) %1 -> b
{-# NOINLINE withUnsafeStrictPerformIO #-}
withUnsafeStrictPerformIO =
  GHC.noinline
    ( Unsafe.toLinear2 \act f -> case GHC.runRW# (GHC.unIO act) of
        (# !_, !a #) ->
          let !b = f a
           in b
    )

freeze :: forall a s. (SV.Storable a) => RW s %1 -> SArray a s -> Ur (SV.Vector a)
{-# NOINLINE freeze #-}
freeze (RW R W) = GHC.noinline $ \(SArray l ptr) ->
  withUnsafeStrictPerformIO (newForeignPtr finalizerFree ptr) \fptr -> Ur (SV.unsafeFromForeignPtr0 fptr l)

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
unsafeGet !r i (SArray _ ptr) =
  (Ur P.<$> peekElemOff ptr i) `withUnsafeStrictPerformIO` (,r)

set :: (SV.Storable a, HasCallStack) => RW s %1 -> Int -> a -> SArray a s -> RW s
{-#OINLINE set #-}
set !rw i a arr@(SArray sz _) =
  if 0 <= i && i < sz
    then unsafeSet rw i a arr
    else error ("get: out of bounds: " <> show (i, sz)) rw

unsafeSet :: (SV.Storable a) => RW s %1 -> Int -> a -> SArray a s -> RW s
{-# NOINLINE unsafeSet #-}
unsafeSet (RW R W) i a (SArray _ ptr) =
  pokeElemOff ptr i a `withUnsafeStrictPerformIO` \() ->
    RW R W

type SlicesTo :: forall {s}. s -> s -> s -> Type
data SlicesTo s l r = SlicesTo

deriveGeneric ''SlicesTo

instance
  (Unsatisfiable ('ShowType (SlicesTo s) ':<>: 'Text " is not Consumable by design")) =>
  Consumable (SlicesTo s l r)
  where
  consume = unsatisfiable
  {-# INLINE consume #-}

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
unsafeSplit (RW R W) lenL (SArray len v) =
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
  SlicesTo
  (RW R W)
  (RW R W)
  (SArray lenL v)
  (SArray lenR _) =
    (Ur (SArray (lenL + lenR) v), RW R W)

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
