{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.AtomicCounter.Linear (
  Counter,
  newCounter,
  newCounterWith,
  getCount,
  increment,
  increment',
  increment_,
  decrement,
  decrement',
  decrement_,
) where

import qualified Control.Monad as P
import Data.Word
import Foreign (Storable, free, peek)
import Foreign.Atomic.Internal
import Foreign.Marshal.Array
import Foreign.Marshal.Pure.Extra (Representable)
import GHC.Exts
import GHC.IO (unsafeDupablePerformIO)
import Linear.Witness.Token
import Prelude.Linear
import System.IO.Unsafe (unsafePerformIO)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

-- Memory layout: |duplicate count|internal count|
newtype Counter = Counter (Ptr Word)
  deriving newtype (Storable, Representable)

-- | FIXME: This is terriblly false!!!
instance Consumable Counter where
  consume = Unsafe.toLinear \(Counter ptr) ->
    unsafePerformIO do
      n <- subFetchWordOffset ptr 0 1
      P.when (n P.== 0) $ free ptr
  {-# NOINLINE consume #-}

-- | FIXME: This is terriblly false!!!
instance Dupable Counter where
  dup2 :: Counter %1 -> (Counter, Counter)
  dup2 = Unsafe.toLinear \c@(Counter ptr) ->
    unsafePerformIO do
      (c, c) P.<$ fetchAddWordOffset ptr 0 1
  {-# NOINLINE dup2 #-}

newCounter :: Linearly %1 -> Counter
{-# INLINE newCounter #-}
newCounter = newCounterWith 0

newCounterWith :: Word -> Linearly %1 -> Counter
{-# NOINLINE newCounterWith #-}
newCounterWith i l =
  l `lseq`
    unsafePerformIO (Counter P.<$!> newArray [1, i])

getCount :: Counter %1 -> (Ur Word, Counter)
{-# NOINLINE getCount #-}
getCount =
  Unsafe.toLinear \(Counter ptr) -> unsafeDupablePerformIO do
    !c <- peek $ ptr `advancePtr` 1
    P.pure (Ur c, Counter ptr)

-- | Increments a counter atomically, and returns the /old/ value.
increment :: Counter %1 -> (Ur Word, Counter)
{-# NOINLINE increment #-}
increment = Unsafe.toLinear \c@(Counter ptr) ->
  unsafePerformIO do
    old <- fetchAddWordOffset ptr 1 1
    P.pure (Ur old, c)

-- | Increments a counter atomically, and returns the /new/ value.
increment' :: Counter %1 -> (Ur Word, Counter)
{-# NOINLINE increment' #-}
increment' = Unsafe.toLinear \c@(Counter ptr) ->
  unsafePerformIO do
    new <- addFetchWordOffset ptr 1 1
    P.pure (Ur new, c)

-- | Increments a counter atomically.
increment_ :: Counter %1 -> Counter
{-# NOINLINE increment_ #-}
increment_ = Unsafe.toLinear \c@(Counter ptr) ->
  unsafePerformIO $
    c P.<$ fetchAddWordOffset ptr 1 1

-- | Decrements a counter atomically, and returns the /old/ value.
decrement :: Counter %1 -> (Ur Word, Counter)
{-# NOINLINE decrement #-}
decrement = Unsafe.toLinear \c@(Counter ptr) ->
  unsafePerformIO do
    old <- fetchSubWordOffset ptr 1 1
    P.pure (Ur old, c)

-- | Decrements a counter atomically, and returns the /new/ value.
decrement' :: Counter %1 -> (Ur Word, Counter)
{-# NOINLINE decrement' #-}
decrement' = Unsafe.toLinear \c@(Counter ptr) ->
  unsafePerformIO do
    old <- subFetchWordOffset ptr 1 1
    P.pure (Ur old, c)

-- | decrements a counter atomically.
decrement_ :: Counter %1 -> Counter
{-# NOINLINE decrement_ #-}
decrement_ = Unsafe.toLinear \c@(Counter ptr) ->
  unsafePerformIO $
    c P.<$ fetchSubWordOffset ptr 1 1
