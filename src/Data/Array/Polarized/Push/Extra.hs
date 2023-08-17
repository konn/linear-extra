{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Array.Polarized.Push.Extra (
  alloc,
  module Data.Array.Polarized.Push,
) where

import qualified Data.Array.Destination.Generic as GD
import Data.Array.Polarized.Push hiding (alloc)
import qualified Data.Array.Polarized.Push as Push
import qualified Data.Vector.Generic as G
import Prelude.Linear
import qualified Prelude as P

data ArrayWriter v a where
  ArrayWriter :: (GD.DArray v a %1 -> ()) %1 -> !Int -> ArrayWriter v a

alloc :: forall v a. G.Vector v a => Push.Array a %1 -> v a
alloc (Push.Array k) = allocArrayWriter $ k singletonWriter
  where
    singletonWriter :: a -> ArrayWriter v a
    singletonWriter a = ArrayWriter (GD.fill a) 1

    allocArrayWriter :: ArrayWriter v a %1 -> v a
    allocArrayWriter (ArrayWriter writer len) = GD.alloc len writer

instance G.Vector v a => P.Semigroup (ArrayWriter v a) where
  x <> y = addWriters x y

instance G.Vector v a => P.Monoid (ArrayWriter v a) where
  mempty = emptyWriter

instance G.Vector v a => Semigroup (ArrayWriter v a) where
  (<>) = addWriters

instance G.Vector v a => Monoid (ArrayWriter v a) where
  mempty = emptyWriter

addWriters :: G.Vector v a => ArrayWriter v a %1 -> ArrayWriter v a %1 -> ArrayWriter v a
addWriters (ArrayWriter k1 l1) (ArrayWriter k2 l2) =
  ArrayWriter
    ( \darr ->
        GD.split l1 darr & \(darr1, darr2) -> consume (k1 darr1, k2 darr2)
    )
    (l1 + l2)

emptyWriter :: G.Vector v a => ArrayWriter v a
emptyWriter = ArrayWriter GD.dropEmpty 0
