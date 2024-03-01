{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

module Data.Vector.Mutable.Nested.Linear (
  PVector (..),
  RW (..),
  R,
  W,
  New (..),
  NewPVector,
  Freeable (..),
  emptyL,
  size,
  capacity,
  lendMut,
  unsafeLendMut,
  lend,
  unsafeLend,
  push,
  foldl',
  ifoldl',
  foldWith,
  ifoldWith,
) where

import qualified Control.Exception as P
import Control.Monad (when)
import Data.Function (fix)
import Data.Kind
import Data.Primitive.Array (MutableArray)
import qualified Data.Primitive.Array as MA
import Data.Primitive.MutVar (MutVar, newMutVar)
import qualified Data.Primitive.MutVar as MV
import Data.Primitive.PrimArray (MutablePrimArray)
import qualified Data.Primitive.PrimArray as PA
import GHC.Exts (Any)
import qualified GHC.Exts as GHC
import qualified GHC.IO as GHC
import qualified GHC.IO as IO
import GHC.Prim
import GHC.Stack (HasCallStack)
import Linear.Token.Borrowing.Unsafe
import Linear.Token.Linearly
import Prelude.Linear hiding (Any, foldl')
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

newtype Some p = Some_ (p Any)

toSome :: p a %1 -> Some p
toSome = Some_ . Unsafe.coerce

type NewPVector p = New (PVector p)

-- | Positional, nested array
type PVector :: (Location -> Type) -> Location -> Type
data PVector p n where
  PVector ::
    {-# UNPACK #-} !(MutablePrimArray RealWorld Int) ->
    -- | N.B. We need indirection here because we want to be able to grow/shrink vectors
    !(MutVar RealWorld (MutableArray RealWorld (Some p))) ->
    PVector p n

instance (Freeable p) => Freeable (PVector p) where
  free = freePArray
  {-# INLINE free #-}

freePArray :: (Freeable p) => RW n %1 -> PVector p n -> ()
{-# NOINLINE freePArray #-}
freePArray (RW r w) vec@(PVector _ arr) =
  size r vec & \(Ur n, r) ->
    unsafeConsumeRW (RW r w) `lseq` unsafeStrictPerformIO do
      fix
        ( \self !i -> when (i < n) do
            Some_ p <- flip MA.readArray i P.=<< MV.readMutVar arr
            () <- P.evaluate (free unsafeRW p)
            self (i + 1)
        )
        0

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

emptyL :: Linearly %1 -> NewPVector p
emptyL l = withUnsafeStrictPerformIO
  ( (,)
      P.<$> PA.unsafeThawPrimArray (PA.primArrayFromList [0])
      P.<*> (newMutVar P.=<< MA.newArray 0 (error "emptyL: uninitialized array element"))
  )
  \(mlen, body) -> unsafeMkNew (PVector mlen body) l
{-# NOINLINE emptyL #-} -- prevents runRW# from floating outwards

size :: R s %1 -> PVector p s -> (Ur Int, R s)
size r (PVector msize _) =
  (Ur (IO.unsafeDupablePerformIO (PA.readPrimArray msize 0)), r)
{-# NOINLINE size #-}

capacity :: R s %1 -> PVector p s -> (Ur Int, R s)
capacity r (PVector _ marr) =
  withUnsafeStrictPerformIO (MA.sizeofMutableArray P.<$> MV.readMutVar marr) \sz ->
    (Ur sz, r)
{-# NOINLINE capacity #-}

unsafeLend ::
  R s %1 ->
  Int ->
  PVector p s ->
  (forall n. R n %1 -> p n -> (a, R n)) %1 ->
  (a, R s)
unsafeLend r i (PVector _ marr) f =
  unsafeConsumeR r `lseq`
    withUnsafeStrictPerformIO (flip MA.readArray i P.=<< MV.readMutVar marr) \case
      Some_ !p ->
        f unsafeR p & \(a, r) ->
          unsafeConsumeR r `lseq` (a, unsafeR)
{-# NOINLINE unsafeLend #-}

lend ::
  (HasCallStack) =>
  R s %1 ->
  Int ->
  PVector p s ->
  (forall n. R n %1 -> p n -> (a, R n)) %1 ->
  (a, R s)
{-# INLINEABLE lend #-}
lend r i pa f =
  size r pa & \(Ur n, r) ->
    if i < n
      then unsafeLend r i pa f
      else error "lend: index out of bounds" r f

unsafeLendMut ::
  RW s %1 ->
  Int ->
  PVector p s ->
  (forall n. RW n %1 -> p n -> (a, RW n)) %1 ->
  (a, RW s)
unsafeLendMut rw i (PVector _ marr) f =
  unsafeConsumeRW rw `lseq`
    withUnsafeStrictPerformIO (flip MA.readArray i P.=<< MV.readMutVar marr) \case
      Some_ !p ->
        f unsafeRW p & \(a, rw) ->
          unsafeConsumeRW rw `lseq` (a, unsafeRW)
{-# NOINLINE unsafeLendMut #-}

lendMut ::
  (HasCallStack) =>
  RW s %1 ->
  Int ->
  PVector p s ->
  (forall n. RW n %1 -> p n -> (a, RW n)) %1 ->
  (a, RW s)
{-# INLINEABLE lendMut #-}
lendMut (RW r w) i pa f =
  size r pa & \(Ur n, r) ->
    if i < n
      then unsafeLendMut (RW r w) i pa f
      else error "lendMut: index out of bounds" r w f

{- | Grows the vector to the closest power of growthFactor to
fit at least n more elements.
-}
growToFit :: RW s %1 -> Int -> PVector a s -> RW s
growToFit (RW r w) n vec =
  capacity r vec & \(Ur cap, r) ->
    size r vec & \(Ur s', r) ->
      if s' + n <= cap
        then RW r w
        else
          let
            -- Calculate the closest power of growth factor
            -- larger than required size.
            newSize =
              constGrowthFactor -- This constant is defined above.
                ^ ceiling @Double @Int
                  ( logBase
                      (fromIntegral constGrowthFactor)
                      (fromIntegral (s' + n)) -- this is always
                      -- > 0 because of
                      -- the if condition
                  )
           in
            unsafeResize (RW r w) newSize vec

constGrowthFactor :: Int
constGrowthFactor = 2

{- | Resize the vector to a non-negative size. In-range elements are preserved,
the possible new elements are bottoms.
-}
unsafeResize :: RW s %1 -> Int -> PVector a s -> RW s
{-# NOINLINE unsafeResize #-}
unsafeResize (RW r w) newSize pv@(PVector msize ma) =
  size r pv & \(Ur size', r) ->
    withUnsafeStrictPerformIO_
      ( do
          -- Update the size reference
          PA.writePrimArray msize 0 (min size' newSize)
          -- Resize the body array
          old <- MV.readMutVar ma
          new <- MA.newArray newSize (error "unsafeResize: uninitialized array element")
          MA.copyMutableArray new 0 old 0 (min size' newSize)
          MV.writeMutVar ma new
      )
      (RW r w)

{- | Insert at the end of the vector. This will grow the vector if there
is no empty space.
-}
push :: RW n %1 -> RW s %1 -> p n -> PVector p s -> RW s
push rwP rw x vec@(PVector s arr) =
  unsafeConsumeRW rwP `lseq`
    growToFit rw 1 vec & \(RW r w) ->
      size r vec & \(Ur !n, r) ->
        withUnsafeStrictPerformIO_
          ( do
              PA.writePrimArray s 0 $! n + 1
              arr <- MV.readMutVar arr
              MA.writeArray arr n (toSome x)
          )
          (RW r w)

foldl' :: R s %1 -> (forall n. a -> R n %1 -> p n -> (Ur a, R n)) -> a -> PVector p s -> (Ur a, R s)
{-# INLINE foldl' #-}
foldl' r f z vec =
  size r vec & \(Ur !n, r) ->
    fix
      ( \self !i !z r ->
          if i < n
            then
              lend r i vec (f z) & \case
                (Ur !z, r) -> self (i + 1) z r
            else (Ur z, r)
      )
      0
      z
      r

ifoldl' :: R s %1 -> (forall n. a -> Int -> R n %1 -> p n -> (Ur a, R n)) -> a -> PVector p s -> (Ur a, R s)
{-# INLINE ifoldl' #-}
ifoldl' r f z vec =
  size r vec & \(Ur !n, r) ->
    fix
      ( \self !i !z r ->
          if i < n
            then
              lend r i vec (f z i) & \case
                (Ur !z, r) -> self (i + 1) z r
            else (Ur z, r)
      )
      0
      z
      r

-- | Strict left-fold, to be used with @purely@ from @foldl@ package.
foldWith ::
  (forall n. RW n %1 -> p n -> a) ->
  RW s %1 ->
  (x -> a -> x) ->
  x ->
  (x -> b) ->
  PVector p s ->
  b
{-# NOINLINE foldWith #-}
foldWith eat (RW r w) g z h vec@(PVector _ marr) =
  size r vec & \(Ur !n, r) ->
    unsafeConsumeRW (RW r w) `lseq`
      fix
        ( \self !i !z ->
            if i < n
              then withUnsafeStrictPerformIO
                (flip MA.readArray i P.=<< MV.readMutVar marr)
                \case
                  Some_ !p ->
                    eat unsafeRW p & \ !x ->
                      self (i + 1) (g z x)
              else h z
        )
        0
        z

-- | Strict indexed left-fold, to be used with @purely@ from @foldl@ package.
ifoldWith ::
  (forall n. Int -> RW n %1 -> p n -> a) ->
  RW s %1 ->
  (x -> a -> x) ->
  x ->
  (x -> b) ->
  PVector p s ->
  b
{-# NOINLINE ifoldWith #-}
ifoldWith eat (RW r w) g z h vec@(PVector _ marr) =
  size r vec & \(Ur !n, r) ->
    unsafeConsumeRW (RW r w) `lseq`
      fix
        ( \self !i !z ->
            if i < n
              then withUnsafeStrictPerformIO
                (flip MA.readArray i P.=<< MV.readMutVar marr)
                \case
                  Some_ !p ->
                    eat i unsafeRW p & \ !x ->
                      self (i + 1) (g z x)
              else h z
        )
        0
        z
