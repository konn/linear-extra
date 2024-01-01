{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fplugin GHC.Magic.Dict.Plugin #-}

module Linear.Token.Borrowing.Unsafe (
  Location,
  R (..),
  W (..),
  unsafeR,
  unsafeW,
  RW (..),
  unsafeRW,
  New (..),
  unsafeMkNew,
  unsafeConsumeRW,
  unsafeConsumeR,
  unsafeConsumeW,
  Freeable (..),
  KnownLocation (..),
  LocAddr (..),
  LocAddr_ (),
  withNewLocation,
) where

import Data.Hashable (Hashable)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Type.Equality
import Data.Unique (Unique, newUnique)
import qualified GHC.Exts as GHC
import qualified GHC.IO as GHC
import GHC.Magic.Dict.Compat (WithDict, withDict)
import GHC.Magic.Dict.Utils (withDictL)
import GHC.TypeLits
import Linear.Token.Linearly (Linearly)
import Linear.Token.Linearly.Unsafe (HasLinearWitness)
import Prelude.Linear hiding (Any)
import Prelude.Linear.Unsatisfiable (Unsatisfiable, unsatisfiable)
import Unsafe.Coerce (unsafeCoerce)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

data Location

type R :: Location -> Type
data R s = R deriving anyclass (HasLinearWitness)

unsafeR :: R s
unsafeR = R

instance
  (Unsatisfiable ('ShowType (R s) ':<>: 'Text " is not Consumable by design")) =>
  Consumable (R s)
  where
  consume = unsatisfiable
  {-# INLINE consume #-}

type W :: Location -> Type
data W s = W deriving anyclass (HasLinearWitness)

unsafeW :: W s
unsafeW = W

instance
  (Unsatisfiable ('ShowType (W s) ':<>: 'Text " is not Consumable by design")) =>
  Consumable (W s)
  where
  consume = unsatisfiable
  {-# INLINE consume #-}

type RW :: Location -> Type
data RW s where
  RW :: !(R s) %1 -> !(W s) %1 -> RW s
  deriving anyclass (HasLinearWitness)

unsafeRW :: RW s
unsafeRW = RW unsafeR unsafeW

instance
  (Unsatisfiable ('ShowType (RW s) ':<>: 'Text " is not Consumable by design")) =>
  Consumable (RW s)
  where
  consume = unsatisfiable
  {-# INLINE consume #-}

type New :: (Location -> Type) -> Type
data New p where
  MkNew :: (KnownLocation s) => p s -> RW s %1 -> New p

unsafeMkNew :: (forall s. p s) -> Linearly %1 -> New p
unsafeMkNew mk lin =
  lin `lseq`
    withNewLocation \(Proxy :: Proxy s) -> MkNew (mk @s) unsafeRW

unsafeConsumeRW :: RW s %1 -> ()
unsafeConsumeRW (RW R W) = ()

unsafeConsumeR :: R s %1 -> ()
unsafeConsumeR R = ()

unsafeConsumeW :: W s %1 -> ()
unsafeConsumeW W = ()

type Freeable :: (Location -> Type) -> Constraint
class Freeable p where
  free :: RW n %1 -> p n -> ()

reifyLoc :: forall s r. LocAddr s -> ((KnownLocation s) => r) %1 -> r
reifyLoc addr k = withDictL @(KnownLocation_ s) addr k

unsafeStrictPerformIO :: IO a %1 -> a
{-# INLINE unsafeStrictPerformIO #-}
unsafeStrictPerformIO = Unsafe.toLinear \act ->
  case GHC.runRW# $ GHC.unIO do GHC.evaluate P.=<< act of
    (# _, !a #) -> GHC.lazy a

newtype WithKnownLoc s r = WithKnownLoc (forall s. (KnownLocation s) => Proxy s -> r)

{- HLINT ignore withNewLocation "Avoid lambda" -}
withNewLocation ::
  forall r.
  (forall s. (KnownLocation s) => Proxy s -> r) %1 ->
  r
{-# NOINLINE withNewLocation #-}
withNewLocation k =
  Unsafe.toLinear
    ( \(WithKnownLoc k) -> unsafeStrictPerformIO do
        !addr :: LocAddr s <- LocAddr . LocAddr_ P.<$> newUnique
        GHC.evaluate $ reifyLoc addr (k @s) Proxy
    )
    (WithKnownLoc k)

type LocAddr :: Location -> Type
newtype LocAddr s = LocAddr {getLocAddr_ :: LocAddr_}
  deriving (P.Eq, P.Ord)
  deriving newtype (Hashable)

type KnownLocation :: Location -> Constraint
class (KnownLocation_ s) => KnownLocation s where
  locAddr :: LocAddr s

instance (KnownLocation_ s) => KnownLocation s where
  locAddr = locAddr_

newtype LocAddr_ = LocAddr_ Unique
  deriving (P.Eq, P.Ord)
  deriving newtype (Hashable)

instance TestEquality LocAddr where
  testEquality (LocAddr i) (LocAddr j) =
    if i P.== j
      then Just (unsafeCoerce Refl)
      else Nothing

type KnownLocation_ :: Location -> Constraint
class KnownLocation_ s where
  locAddr_ :: LocAddr s
