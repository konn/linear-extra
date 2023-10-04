{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
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
  increment_,
  decrement,
  decrement_,
) where

import qualified Data.Functor.Linear as D
import qualified Data.Replicator.Linear as Rep
import Data.Word
import Foreign (Storable)
import Foreign.Atomic.Internal
import Foreign.Marshal.Pure.Extra (Box, Pool, Representable)
import qualified Foreign.Marshal.Pure.Extra as Box
import qualified Foreign.Marshal.Pure.Internal as Box
import GHC.Exts
import GHC.IO (unIO)
import Prelude.Linear
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

newtype Counter = Counter (Box Word)
  deriving newtype (Storable, Representable)

-- | FIXME: This is terriblly false!!!
instance Consumable Counter where
  consume = Unsafe.toLinear (`P.seq` ())

-- | FIXME: This is terriblly false!!!
instance Dupable Counter where
  dupR = Unsafe.toLinear Rep.pure
  {-# INLINE dupR #-}

newCounter :: Pool %1 -> Counter
{-# INLINE newCounter #-}
newCounter = newCounterWith 0

newCounterWith :: Word -> Pool %1 -> Counter
{-# NOINLINE newCounterWith #-}
newCounterWith i p = Counter (Box.alloc i p)

getCount :: Counter %1 -> (Ur Word, Counter)
{-# NOINLINE getCount #-}
getCount (Counter b) =
  case runRW# (unIO synchronise) of
    (# !_, () #) -> Counter D.<$> Box.get b

-- | Increments a counter atomically, and returns the /old/ value.
increment :: Counter %1 -> (Ur Word, Counter)
{-# NOINLINE increment #-}
increment = Unsafe.toLinear \c@(Counter (Box.Box _ ptr)) ->
  case runRW# (unIO $ fetchAddWordOffset ptr 0 1) of
    (# !_, w #) -> (Ur w, c)

-- | Increments a counter atomically.
increment_ :: Counter %1 -> Counter
{-# NOINLINE increment_ #-}
increment_ = Unsafe.toLinear \c@(Counter (Box.Box _ ptr)) ->
  case runRW# (unIO $ fetchAddWordOffset ptr 0 1) of
    (# !_, _ #) -> c

-- | decrements a counter atomically, and returns the /old/ value.
decrement :: Counter %1 -> (Ur Word, Counter)
{-# NOINLINE decrement #-}
decrement = Unsafe.toLinear \c@(Counter (Box.Box _ ptr)) ->
  case runRW# (unIO $ fetchSubWordOffset ptr 0 1) of
    (# !_, w #) -> (Ur w, c)

-- | decrements a counter atomically.
decrement_ :: Counter %1 -> Counter
{-# NOINLINE decrement_ #-}
decrement_ = Unsafe.toLinear \c@(Counter (Box.Box _ ptr)) ->
  case runRW# (unIO $ fetchSubWordOffset ptr 0 1) of
    (# !_, _ #) -> c
