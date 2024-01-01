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
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Linear.Token.Borrowing.Owner.Typed.Internal (
  module Linear.Token.Borrowing.Owner.Typed.Internal,
) where

import Data.Kind
import Data.Type.Bool
import Data.Type.Equality
import GHC.Exts
import Linear.Token.Borrowing.Unsafe
import Linear.Token.Linearly
import Prelude.Linear

type TokenRep = 'TupleRep ('[] :: [RuntimeRep])

type TokenType = TYPE TokenRep

type Owner :: [Location] -> [Location] -> TokenType
newtype Owner rs ws = Owner_ (# #)

consumeOwner :: (rs ~ '[], ws ~ '[]) => Owner rs ws %1 -> ()
consumeOwner (Owner_ (# #)) = ()

newOwner :: Linearly %1 -> Owner '[] '[]
{-# INLINE newOwner #-}
newOwner lin = consume lin & \() -> Owner_ (# #)

lendMut ::
  (Member s rs, Member s ws) =>
  Owner rs ws %1 ->
  (# RW s, Owner (Delete s rs) (Delete s ws) #)
lendMut (Owner_ (# #)) = (# unsafeRW, Owner_ (# #) #)

lend ::
  (Member s rs) =>
  Owner rs ws %1 ->
  (# R s, Owner (Delete s rs) ws #)
lend (Owner_ (# #)) = (# unsafeR, Owner_ (# #) #)

unlend ::
  (Absent s rs) =>
  R s %1 ->
  Owner rs ws %1 ->
  Owner (s ': rs) ws
unlend r (Owner_ (# #)) = unsafeConsumeR r & \() -> Owner_ (# #)

unlendMut ::
  (Absent s rs, Absent s ws) =>
  RW s %1 ->
  Owner rs ws %1 ->
  Owner (s ': rs) (s ': ws)
unlendMut rw (Owner_ (# #)) = unsafeConsumeRW rw & \() -> Owner_ (# #)

type SLocs :: [Location] -> Type
data SLocs ls where
  SNil :: SLocs '[]
  (:-) :: LocAddr s -> SLocs ls -> SLocs (s ': ls)

infixr 9 :-

type KnownLocations_ :: [Location] -> Constraint
type family KnownLocations_ ls where
  KnownLocations_ '[] = ()
  KnownLocations_ (l ': ls) = (KnownLocation l, KnownLocations ls)

type KnownLocations :: [Location] -> Constraint
class (KnownLocations_ ls) => KnownLocations ls where
  sLocs :: SLocs ls

instance KnownLocations '[] where
  sLocs = SNil
  {-# INLINE sLocs #-}

instance (KnownLocation s, KnownLocations ls) => KnownLocations (s ': ls) where
  sLocs = locAddr :- sLocs
  {-# INLINE sLocs #-}

type Absent x xs = Elem x xs ~ 'False

type Member x xs = Elem x xs ~ 'True

type IsJust :: forall {k}. Maybe k -> Bool
type family IsJust m where
  IsJust ('Just _) = 'True
  IsJust 'Nothing = 'False

type Elem :: forall {x}. x -> [x] -> Bool
type family Elem x xs where
  Elem _ '[] = 'False
  Elem x (y ': ys) =
    If (x == y) 'True (Elem x ys)

type Delete :: forall {x}. x -> [x] -> [x]
type family Delete x xs where
  Delete x (y ': xs) = If (x == y) xs (y ': Delete x xs)
  Delete _ '[] = '[]
