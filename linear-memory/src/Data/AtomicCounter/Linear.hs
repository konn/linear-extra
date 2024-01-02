{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v0 #-}

module Data.AtomicCounter.Linear (
  Counter,
  withCounter,
  withCounterCapacity,
  getCount,
  increment,
  increment',
  increment_,
  decrement,
  decrement',
  decrement_,
) where

import Control.Exception (evaluate, finally)
import qualified Control.Monad as P
import Data.Word
import Foreign (free, peek, poke)
import qualified Foreign
import Foreign.Atomic.Internal
import Foreign.Marshal.Array
import Foreign.Marshal.Pure (MkRepresentable (..), Representable (..))
import Foreign.Storable.Generic (GStorable)
import GHC.Exts
import GHC.Generics (Generic)
import GHC.IO (uninterruptibleMask_, unsafeDupablePerformIO)
import Prelude.Linear
import System.IO.Unsafe (unsafePerformIO)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

-- | Thread-safe atomic counter.
data Counter
  = Counter
      {-# UNPACK #-} !(Ptr Word)
      -- ^ Counter body.
      --  Memory layout: | number of duplicated counters | count |
      {-# UNPACK #-} !(Ptr Bool)
      -- ^ Set to @1@ if already released.
      -- Used in 'withCounterCapacity' to ensure exception resilience.
  deriving (Generic)
  deriving anyclass (GStorable)

instance Representable Counter where
  type AsKnown Counter = AsKnown (Ptr Word, Ptr Bool)

instance MkRepresentable Counter (Ptr Word, Ptr Bool) where
  toRepr (Counter i p) = (i, p)
  {-# INLINE toRepr #-}
  ofRepr (i, p) = Counter i p
  {-# INLINE ofRepr #-}

instance Consumable Counter where
  consume = Unsafe.toLinear \(Counter ptr dst) ->
    unsafePerformIO $ uninterruptibleMask_ do
      n <- subFetchWordOffset ptr 0 1
      P.when (n P.== 0) do
        free ptr
        poke dst True
  {-# NOINLINE consume #-}

instance Dupable Counter where
  dup2 :: Counter %1 -> (Counter, Counter)
  dup2 = Unsafe.toLinear \c@(Counter ptr _) ->
    unsafePerformIO do
      (c, c) P.<$ fetchAddWordOffset ptr 0 1
  {-# NOINLINE dup2 #-}

withCounter :: (Counter %1 -> Ur a) %1 -> Ur a
{-# INLINE withCounter #-}
withCounter = withCounterCapacity 0

withCounterCapacity :: Word -> (Counter %1 -> Ur a) %1 -> Ur a
{-# NOINLINE withCounterCapacity #-}
withCounterCapacity i = Unsafe.toLinear \k -> unsafeDupablePerformIO do
  ptr <- newArray [1, i]
  releasedP <- Foreign.new False
  evaluate (k (Counter ptr releasedP)) `finally` do
    released <- peek releasedP
    P.unless released $ do
      free ptr
      free releasedP

getCount :: Counter %1 -> (Ur Word, Counter)
{-# NOINLINE getCount #-}
getCount =
  Unsafe.toLinear \ctr@(Counter ptr _) -> unsafeDupablePerformIO do
    !c <- peek $ ptr `advancePtr` 1
    P.pure (Ur c, ctr)

-- | Increments a counter atomically, and returns the /old/ value.
increment :: Counter %1 -> (Ur Word, Counter)
{-# NOINLINE increment #-}
increment = Unsafe.toLinear \c@(Counter ptr _) ->
  unsafePerformIO do
    old <- fetchAddWordOffset ptr 1 1
    P.pure (Ur old, c)

-- | Increments a counter atomically, and returns the /new/ value.
increment' :: Counter %1 -> (Ur Word, Counter)
{-# NOINLINE increment' #-}
increment' = Unsafe.toLinear \c@(Counter ptr _) ->
  unsafePerformIO do
    new <- addFetchWordOffset ptr 1 1
    P.pure (Ur new, c)

-- | Increments a counter atomically.
increment_ :: Counter %1 -> Counter
{-# NOINLINE increment_ #-}
increment_ = Unsafe.toLinear \c@(Counter ptr _) ->
  unsafePerformIO $
    c P.<$ fetchAddWordOffset ptr 1 1

-- | Decrements a counter atomically, and returns the /old/ value.
decrement :: Counter %1 -> (Ur Word, Counter)
{-# NOINLINE decrement #-}
decrement = Unsafe.toLinear \c@(Counter ptr _) ->
  unsafePerformIO do
    old <- fetchSubWordOffset ptr 1 1
    P.pure (Ur old, c)

-- | Decrements a counter atomically, and returns the /new/ value.
decrement' :: Counter %1 -> (Ur Word, Counter)
{-# NOINLINE decrement' #-}
decrement' = Unsafe.toLinear \c@(Counter ptr _) ->
  unsafePerformIO do
    old <- subFetchWordOffset ptr 1 1
    P.pure (Ur old, c)

-- | decrements a counter atomically.
decrement_ :: Counter %1 -> Counter
{-# NOINLINE decrement_ #-}
decrement_ = Unsafe.toLinear \c@(Counter ptr _) ->
  unsafePerformIO $
    c P.<$ fetchSubWordOffset ptr 1 1
