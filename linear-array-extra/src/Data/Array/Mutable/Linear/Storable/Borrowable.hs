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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Data.Array.Mutable.Linear.Storable.Borrowable (
  SArray (),
  allocL,
  fromListL,
  fromVectorL,
  freeze,
  R,
  W,
  RW (..),
  SuchThat (..),
  size,
  get,
  unsafeGet,
  set,
  unsafeSet,
  SlicesTo (),
  Slice (..),
  split,
  halve,
  unsafeSplit,
  combine,
) where

import qualified Data.Array.Mutable.Linear.Storable as Raw
import qualified Data.Array.Mutable.Linear.Storable.Internal as Raw
import Data.Coerce (coerce)
import Data.Kind (Type)
import qualified Data.Vector.Storable as SV
import Foreign.Marshal.Array (advancePtr)
import Foreign.Storable (Storable)
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Generics.Linear.TH (deriveGeneric)
import Linear.Witness.Token
import Prelude.Linear
import Prelude.Linear.Generically (Generically (..))
import Prelude.Linear.Unsatisfiable (Unsatisfiable, unsatisfiable)
import qualified Unsafe.Linear as Unsafe

{- HLINT ignore SuchThat "Redundant bracket" -}
type SuchThat :: forall {s}. (s -> Type) -> (s -> Type) -> s -> Type
data SuchThat f g s where
  SuchThat :: f s -> g s %1 -> SuchThat f g s

newtype SArray a s = SArray (Raw.SArray a)

type R :: forall {s}. s -> Type
data R s = R

deriveGeneric ''R

deriving via Generically (R s) instance Consumable (R s)

instance
  (Unsatisfiable ('ShowType (R s) ':<>: 'Text " is not Dupable by design")) =>
  Dupable (R s)
  where
  dup2 = unsatisfiable
  {-# INLINE dup2 #-}
  dupR = unsatisfiable
  {-# INLINE dupR #-}

type W :: forall {s}. s -> Type
data W s = W

deriveGeneric ''W

deriving via Generically (W s) instance Consumable (W s)

instance
  (Unsatisfiable ('ShowType (W s) ':<>: 'Text " is not Dupable by design")) =>
  Dupable (W s)
  where
  dup2 = unsatisfiable
  {-# INLINE dup2 #-}
  dupR = unsatisfiable
  {-# INLINE dupR #-}

type RW :: forall {s}. s -> Type
data RW s where
  RW :: R s %1 -> W s %1 -> RW s

deriveGeneric ''RW

deriving via Generically (RW s) instance Consumable (RW s)

instance
  (Unsatisfiable ('ShowType (RW s) ':<>: 'Text " is not Dupable by design")) =>
  Dupable (RW s)
  where
  dup2 = unsatisfiable
  {-# INLINE dup2 #-}
  dupR = unsatisfiable
  {-# INLINE dupR #-}

allocL ::
  (HasCallStack, SV.Storable a) =>
  Int ->
  a ->
  Linearly %1 ->
  SuchThat (SArray a) RW s
{-# INLINE allocL #-}
allocL s x l =
  Raw.allocL s x l & Unsafe.toLinear \rawArr ->
    SArray rawArr `SuchThat` RW R W

fromListL :: (Storable a) => [a] -> Linearly %1 -> SuchThat (SArray a) RW s
{-# INLINE fromListL #-}
fromListL xs l =
  Raw.fromListL xs l & Unsafe.toLinear \rawArr ->
    SArray rawArr `SuchThat` RW R W

fromVectorL :: (Storable a) => SV.Vector a -> Linearly %1 -> SuchThat (SArray a) RW s
{-# INLINE fromVectorL #-}
fromVectorL xs l =
  Raw.fromVectorL xs l & Unsafe.toLinear \rawArr ->
    SArray rawArr `SuchThat` RW R W

freeze :: forall a s. (SV.Storable a) => RW s %1 -> SArray a s -> Ur (SV.Vector a)
{-# INLINE freeze #-}
freeze = (`lseq` coerce (forget $ Raw.freeze @a))

size :: R s %1 -> SArray a s -> (Ur Int, R s)
{-# INLINE size #-}
size r (SArray sa) = Raw.size sa & \(len, _sa) -> (len, r)

get :: (SV.Storable a, HasCallStack) => R s %1 -> Int -> SArray a s -> (Ur a, R s)
{-# INLINE get #-}
get r i (SArray sa) =
  Raw.get i sa & \(a, _) -> (a, r)

unsafeGet :: (SV.Storable a) => R s %1 -> Int -> SArray a s -> (Ur a, R s)
{-# INLINE unsafeGet #-}
unsafeGet r i (SArray sa) =
  Raw.unsafeGet i sa & \(a, _) -> (a, r)

set :: (SV.Storable a, HasCallStack) => RW s %1 -> Int -> a -> SArray a s -> RW s
{-# INLINE set #-}
set rw i a (SArray sa) =
  Raw.set i a sa & \_ -> rw

unsafeSet :: (SV.Storable a) => RW s %1 -> Int -> a -> SArray a s -> RW s
{-# INLINE unsafeSet #-}
unsafeSet rw i a (SArray sa) =
  Raw.unsafeSet i a sa & \_ -> rw

type SlicesTo :: forall {s}. s -> s -> s -> Type
data SlicesTo s l r = SlicesTo

deriveGeneric ''SlicesTo

deriving via Generically (SlicesTo s l r) instance Consumable (SlicesTo s l r)

instance
  (Unsatisfiable ('ShowType (SlicesTo s) ':<>: 'Text " is not Dupable by design")) =>
  Dupable (SlicesTo s l r)
  where
  dup2 = unsatisfiable
  {-# INLINE dup2 #-}
  dupR = unsatisfiable
  {-# INLINE dupR #-}

data Slice a s where
  MkSlice ::
    SlicesTo s l r %1 ->
    RW l %1 ->
    RW r %1 ->
    SArray a l ->
    SArray a r ->
    Slice a s

unsafeSplit :: (SV.Storable a) => RW s %1 -> Int -> SArray a s -> Slice a s
{-# INLINE unsafeSplit #-}
unsafeSplit (RW R W) lenL (SArray (Raw.SArray len v)) =
  let !lenR = len - lenL
   in MkSlice
        SlicesTo
        (RW R W)
        (RW R W)
        (SArray (Raw.SArray lenL v))
        (SArray (Raw.SArray lenR (v `advancePtr` lenL)))

split :: (SV.Storable a, HasCallStack) => RW s %1 -> Int -> SArray a s -> Slice a s
{-# INLINE split #-}
split (RW r w) l arr =
  size r arr & \(Ur n, r) ->
    if 0 <= l && l < n
      then unsafeSplit (RW r w) l arr
      else r `lseq` w `lseq` error ("split: Index out of bounds: " <> show (l, n))

combine ::
  SlicesTo s l r %1 ->
  RW l %1 ->
  RW r %1 ->
  SArray a l ->
  SArray a r ->
  (Ur (SArray a s), RW s)
{-# INLINE combine #-}
combine
  SlicesTo
  (RW R W)
  (RW R W)
  (SArray (Raw.SArray lenL v))
  (SArray (Raw.SArray lenR _)) =
    (Ur (SArray (Raw.SArray (lenL + lenR) v)), RW R W)

halve :: (SV.Storable a) => RW s %1 -> SArray a s -> Slice a s
{-# INLINE halve #-}
halve (RW r w) arr =
  size r arr & \(Ur ln, r) ->
    unsafeSplit (RW r w) (ln `quot` 2) arr
