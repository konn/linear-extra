{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

module Linear.Token.Borrowing.Owner.Typed (
  Owner (),
  consumeOwner,
  lseq,
  newOwner,
  lending,
  lendingMut,
  lend,
  lendMut,
  unlend,
  unlendMut,
  KnownLocations,
  sLocs,
  SLocs (..),
  KnownLocation (..),
  sInsert,
  sDelete,
  sMember,
  Existence (..),
) where

import Data.Kind
import Data.Type.Equality
import Linear.Token.Borrowing (KnownLocation (..))
import Linear.Token.Borrowing.Owner.Typed.Internal
import Linear.Token.Borrowing.Unsafe (LocAddr, Location)
import Prelude.Linear hiding (lseq)
import Unsafe.Coerce (unsafeCoerce)

sInsert :: LocAddr s -> SLocs ss -> SLocs (s ': ss)
sInsert = (:-)

sDelete :: LocAddr s -> SLocs ss -> SLocs (Delete s ss)
sDelete _ SNil = SNil
sDelete l (r :- ls) =
  case sEqLocAddr l r of
    STrue -> ls
    SFalse -> r :- sDelete l ls

sMember :: LocAddr s -> SLocs ss -> Existence s ss
sMember _ SNil = Absent
sMember l (r :- rs) =
  case sEqLocAddr l r of
    STrue -> Present
    SFalse -> case sMember l rs of
      Present -> Present
      Absent -> Absent

type Existence :: Location -> [Location] -> Type
data Existence l ls where
  Present :: (Member l ls) => Existence l ls
  Absent :: (Absent l ls) => Existence l ls

type SBool :: Bool -> Type
data SBool p where
  SFalse :: SBool 'False
  STrue :: SBool 'True

sEqLocAddr :: LocAddr s -> LocAddr t -> SBool (s == t)
sEqLocAddr l r = case testEquality l r of
  Just Refl -> unsafeCoerce STrue
  Nothing -> unsafeCoerce SFalse
