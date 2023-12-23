{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
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
) where

import Data.Kind (Type)
import GHC.TypeLits
import Linear.Token.Linearly (Linearly)
import Linear.Token.Linearly.Unsafe (HasLinearWitness)
import Prelude.Linear hiding (Any)
import Prelude.Linear.Unsatisfiable (Unsatisfiable, unsatisfiable)

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
  MkNew :: p s -> RW s %1 -> New p

unsafeMkNew :: (forall s. p s) -> Linearly %1 -> New p
unsafeMkNew mk lin = lin `lseq` MkNew mk unsafeRW

unsafeConsumeRW :: RW s %1 -> ()
unsafeConsumeRW (RW R W) = ()
