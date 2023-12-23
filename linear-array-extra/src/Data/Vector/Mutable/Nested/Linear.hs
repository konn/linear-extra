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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

module Data.Vector.Mutable.Nested.Linear (
  PArray (..),
  RW (..),
  R,
  W,
  New (..),
  NewPArray,
  emptyL,
  size,
  lendMut,
  unsafeLendMut,
  lend,
  unsafeLend,
) where

import Data.Kind
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
import Prelude.Linear hiding (Any)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

newtype Some p = Some_ (p Any)

toSome :: p a %1 -> Some p
toSome = Some_ . Unsafe.coerce

withSome :: Some p %1 -> (forall a. p a %1 -> b) %1 -> b
withSome (Some_ p) f = f (Unsafe.coerce p)

type NewPArray p = New (PArray p)

-- | Positional, nested array
type PArray :: (Location -> Type) -> Location -> Type
data PArray p n where
  PArray ::
    {-# UNPACK #-} !(MutablePrimArray RealWorld Int) ->
    !(MutableArray# RealWorld (Some p)) ->
    PArray p n

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

emptyL :: Linearly %1 -> NewPArray p
emptyL l = case GHC.runRW# (newArray# 0# undefined) of
  (# !_, marr #) ->
    withUnsafeStrictPerformIO
      (PA.unsafeThawPrimArray (PA.primArrayFromList [0]))
      \pa -> unsafeMkNew (PArray pa marr) l
{-# NOINLINE emptyL #-} -- prevents runRW# from floating outwards

size :: R s %1 -> PArray p s -> (Ur Int, R s)
size r (PArray msize _) =
  (Ur (IO.unsafePerformIO (PA.readPrimArray msize 0)), r)
{-# NOINLINE size #-}

unsafeLend ::
  R s %1 ->
  Int ->
  PArray p s ->
  (forall n. R n %1 -> p n -> (a, R n)) %1 ->
  (a, R s)
unsafeLend r (GHC.I# i) (PArray _ marr) f =
  unsafeConsumeR r `lseq` case GHC.runRW# (readArray# marr i) of
    (# !_, Some_ !p #) ->
      f unsafeR p & \(a, r) ->
        unsafeConsumeR r `lseq` (a, unsafeR)
{-# NOINLINE unsafeLend #-}

lend ::
  (HasCallStack) =>
  R s %1 ->
  Int ->
  PArray p s ->
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
  PArray p s ->
  (forall n. RW n %1 -> p n -> (a, RW n)) %1 ->
  (a, RW s)
unsafeLendMut rw (GHC.I# i) (PArray _ marr) f =
  unsafeConsumeRW rw `lseq` case GHC.runRW# (readArray# marr i) of
    (# !_, Some_ !p #) ->
      f unsafeRW p & \(a, rw) ->
        unsafeConsumeRW rw `lseq` (a, unsafeRW)
{-# NOINLINE unsafeLendMut #-}

lendMut ::
  (HasCallStack) =>
  RW s %1 ->
  Int ->
  PArray p s ->
  (forall n. RW n %1 -> p n -> (a, RW n)) %1 ->
  (a, RW s)
{-# INLINEABLE lendMut #-}
lendMut (RW r w) i pa f =
  size r pa & \(Ur n, r) ->
    if i < n
      then unsafeLendMut (RW r w) i pa f
      else error "lendMut: index out of bounds" r w f
