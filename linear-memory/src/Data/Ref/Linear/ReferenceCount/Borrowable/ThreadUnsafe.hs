{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v0 #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Ref.Linear.ReferenceCount.Borrowable.ThreadUnsafe where

import Data.AtomicCounter.Linear (Counter, decrement', decrement_, newCounter)
import Data.Kind
import qualified Data.Replicator.Linear as Rep
import Data.Word
import Foreign (Ptr, Storable (..), castPtr)
import Foreign.Marshal.Pure (Box, Pool, Representable)
import Foreign.Marshal.Pure.Extra ()
import Foreign.Marshal.Pure.Internal (Box (..), Pool (..), insertAfter, reprNew)
import Foreign.Storable.Generic (GStorable)
import qualified GHC.Exts as GHC
import GHC.Generics (Generic)
import qualified GHC.IO as IO
import Linear.Token.Borrowing.Unsafe
import Linear.Token.Linearly.Unsafe (linearWitness)
import Prelude.Linear
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

-- | Memory layout: |strong|weak|body|
newtype RcBox a = RcBox (# Counter, Counter, Box a #)

data RcBox' a = RcBox' Counter Counter (Box a)
  deriving (Generic)
  deriving anyclass (GStorable)

fromRcBox' :: RcBox' a -> RcBox a
fromRcBox' (RcBox' strong weak body) = RcBox (# strong, weak, body #)

toRcBox' :: RcBox a -> RcBox' a
toRcBox' (RcBox (# strong, weak, body #)) = RcBox' strong weak body

instance (Storable a) => Storable (Rc a s) where
  sizeOf _ = sizeOf (undefined :: (RcBox' a))
  alignment _ = alignment (undefined :: RcBox' a)
  peek = P.fmap (\x -> Rc (fromRcBox' x)) P.. peek P.. castPtr
  poke ptr = \(Rc r) -> poke (castPtr ptr) (toRcBox' r)

instance (Storable a) => Storable (Weak a s) where
  sizeOf _ = sizeOf (undefined :: (RcBox' a))
  alignment _ = alignment (undefined :: RcBox' a)
  peek = P.fmap (\x -> Weak (fromRcBox' x)) P.. peek P.. castPtr
  poke ptr = \(Weak r) -> poke (castPtr ptr) (toRcBox' r)

alloc :: forall a. (Representable a) => a %1 -> Pool %1 -> New (Rc a)
{-# NOINLINE alloc #-}
alloc a0 pool =
  Rep.elim @3
    ( \p1 p2 pool ->
        newCounter p1 & \strong ->
          newCounter p2 & \weak ->
            Unsafe.toLinear (mkPtr strong weak pool) a0
    )
    (dupR pool)
  where
    mkPtr :: Counter %1 -> Counter %1 -> Pool %1 -> a -> New (Rc a)
    mkPtr = Unsafe.toLinear3 \strong weak pool a ->
      linearWitness pool
        & Unsafe.toLinear \(Pool pool, l) ->
          withUnsafeStrictPerformIO
            ( do
                ptr <- reprNew a
                poolPtr <- insertAfter pool (castPtr ptr :: Ptr ())
                P.pure (ptr, poolPtr)
            )
            \(ptr, poolPtr) ->
              unsafeMkNew (Rc (RcBox (# strong, weak, Box poolPtr ptr #))) l

withUnsafeStrictPerformIO_ :: IO () -> a %1 -> a
{-# INLINE withUnsafeStrictPerformIO_ #-}
withUnsafeStrictPerformIO_ act = Unsafe.toLinear \x ->
  case GHC.runRW# (IO.unIO (do do () <- act; P.pure x)) of
    (# _, !a #) -> GHC.lazy a

unsafeStrictPerformIO :: IO a %1 -> a
{-# INLINE unsafeStrictPerformIO #-}
unsafeStrictPerformIO = Unsafe.toLinear \act ->
  case GHC.runRW# (IO.unIO do IO.evaluate P.=<< act) of
    (# _, !a #) -> GHC.lazy a

withUnsafeStrictPerformIO :: IO a %1 -> (a -> b) %1 -> b
{-# INLINE withUnsafeStrictPerformIO #-}
withUnsafeStrictPerformIO = Unsafe.toLinear2 \act f ->
  case GHC.runRW# (IO.unIO do !a <- act; IO.evaluate (f a)) of
    (# _, b #) -> GHC.lazy b

{- |
A reference-counted mutable cell, allocated off-heap.

__This is thread-unsafe__.
-}
type Rc :: Type -> Location -> Type
data Rc a s where
  Rc :: {-# UNPACK #-} !(RcBox a) -> Rc a s

instance Freeable (Rc a) where
  free = freeRcBox
  {-# INLINE free #-}

freeRcBox :: RW s %1 -> Rc a s -> ()
freeRcBox rw (Rc (RcBox (# strong, weak, b #))) =
  unsafeConsumeRW rw `lseq` go (decrement' strong) weak b
  where
    -- FIXME: when making strong and weak allocated off-heap
    -- consider weak count and release them accordingly
    go :: (Ur Word, Counter) %1 -> Counter %1 -> Box a %1 -> ()
    go (Ur 0, strong) weak = \b ->
      b `lseq` decrement_ weak `lseq` strong `lseq` ()
    go (Ur _, strong) weak = Unsafe.toLinear \_ ->
      strong `lseq` weak `lseq` ()

{- |
A weak reference to some 'Rc'.
__This is thread-unsafe__.

Use 'downgrade' and 'upgrade' to convert from/to 'Rc'.
-}
type Weak :: Type -> Location -> Type
data Weak a s where
  Weak :: {-# UNPACK #-} !(RcBox a) -> Weak a s

freeWeak :: RW s %1 -> Weak a s -> ()
freeWeak rw (Weak (RcBox (# _, weak, _ #))) =
  unsafeConsumeRW rw `lseq` go weak
  where
    -- FIXME: when making strong and weak allocated off-heap
    -- consider weak count and release them accordingly
    go :: Counter %1 -> ()
    go weak = decrement_ weak `lseq` ()

instance Freeable (Weak a) where
  free = freeWeak
  {-# INLINE free #-}
