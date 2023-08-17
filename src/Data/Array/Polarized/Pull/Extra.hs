{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Array.Polarized.Pull.Extra (
  fromVector,
  module Data.Array.Polarized.Pull,
) where

import Data.Array.Polarized.Pull hiding (fromVector)
import qualified Data.Vector.Generic as G
import qualified Unsafe.Linear as Unsafe

fromVector :: G.Vector v a => v a %1 -> Array a
fromVector = Unsafe.toLinear \v -> fromFunction (G.unsafeIndex v) (G.length v)
