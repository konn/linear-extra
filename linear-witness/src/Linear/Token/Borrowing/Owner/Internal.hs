{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fplugin GHC.Magic.Dict.Plugin #-}

module Linear.Token.Borrowing.Owner.Internal (
  Owner (..),
  SomeOwner (..),
  newOwner,
  Ownership (..),
  withOwnership,
  Owns (),
  Owns_ (..),
  type (∈),
  moveTo,
  Moved (..),
  tryLend,
  tryLendMut,
  unlend,
  unlendMut,
  unsafeMkNewInOwner,
  unsafeWithOwns,
  OwnedBy (..),
) where

import qualified Data.Bifunctor.Linear as BiL
import qualified Data.HashMap.Mutable.Linear as LHM
import qualified Data.HashMap.Mutable.Linear.Witness as LHM
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.Magic.Dict.Utils (withDictL)
import Linear.Token.Borrowing.Unsafe
import Linear.Token.Linearly
import Prelude.Linear

data SomeOwner where
  MkSomeOwner :: Owner s %1 -> SomeOwner

data LocState = LendRead | LendWrite | ReadWrite

newtype Owner s = Owner (LHM.HashMap LocAddr_ LocState)
  deriving newtype (Consumable)

newOwner :: Linearly %1 -> SomeOwner
newOwner l = MkSomeOwner (Owner (LHM.emptyL 16 l))

withOwnership ::
  forall s n r.
  (KnownLocation n) =>
  Ownership s n ->
  ((Owns s n) => r) %1 ->
  r
withOwnership ship k = withDictL @(Owns_ s n) ship (k :: (Owns_ s n) => r)

type Ownership :: Location -> Location -> Type
data Ownership s n = Ownership

type s ∈ n = n `Owns` s

class (KnownLocation n, Owns_ s n) => Owns s n

instance (KnownLocation n, Owns_ s n) => Owns s n

type Owns_ :: Location -> Location -> Constraint
class Owns_ s n where
  _owns :: Ownership s n

tryLendMut :: forall s n. (s ∈ n) => Owner n %1 -> (Maybe (RW s), Owner s)
tryLendMut (Owner src) =
  BiL.second Owner $
    LHM.alterF
      ( \case
          Just ReadWrite -> (Just unsafeRW, Ur (Just LendWrite))
          _ -> (Nothing, Ur Nothing)
      )
      (getLocAddr_ @s locAddr)
      src

tryLend :: forall s n. (s ∈ n) => Owner n %1 -> (Maybe (R s), Owner s)
tryLend (Owner src) =
  BiL.second Owner $
    LHM.alterF
      ( \case
          Just ReadWrite -> (Just unsafeR, Ur (Just LendRead))
          _ -> (Nothing, Ur Nothing)
      )
      (getLocAddr_ @s locAddr)
      src

unlend :: forall s n. (s ∈ n) => R s %1 -> Owner n %1 -> Owner n
unlend R (Owner src) =
  Owner $
    LHM.alter
      (const $ Just ReadWrite)
      (getLocAddr_ @s locAddr)
      src

unlendMut :: forall s n. (s ∈ n) => RW s %1 -> Owner n %1 -> Owner n
unlendMut (RW R W) (Owner src) =
  Owner $
    LHM.alter
      (const $ Just ReadWrite)
      (getLocAddr_ @s locAddr)
      src

unsafeWithOwns :: forall s n r. (KnownLocation n) => ((Owns s n) => r) %1 -> r
{- HLINT ignore unsafeWithOwns -}
unsafeWithOwns k = withDictL @(Owns_ s n) (Ownership @s @n) k

data Moved s n where
  MkMoved :: (s `Owns` n) => Owner s %1 -> Moved s n

moveTo :: forall s n. (KnownLocation n) => RW n %1 -> Owner s %1 -> Moved s n
moveTo rw owner = unsafeWithOwns @s @n (MkMoved @s @n (unlendMut rw owner))

data (p `OwnedBy` s) n where
  Owned :: (s `Owns` n) => p n -> (p `OwnedBy` s) n

unsafeMkNewInOwner ::
  forall n p.
  (forall s. p s) ->
  Owner n %1 ->
  (New (p `OwnedBy` n), Owner n)
unsafeMkNewInOwner mk (Owner dic) =
  withNewLocation \(Proxy :: Proxy s) ->
    LHM.insert
      (getLocAddr_ @s locAddr)
      LendWrite
      dic
      & \dic ->
        unsafeWithOwns
          @n
          @s
          (MkNew (Owned $ mk @s) unsafeRW, Owner dic)
