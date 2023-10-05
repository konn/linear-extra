{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Array.Mutable.Unlifted.Linear.Witness (
  allocL,
) where

import Data.Array.Mutable.Unlifted.Linear
import GHC.Exts (unsafeCoerce#)
import qualified GHC.Exts as GHC
import Linear.Witness.Token
import Prelude.Linear
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

allocL :: Int -> a -> Linearly %1 -> Array# a
{-# ANN allocL "HLint: ignore Avoid lambda" #-}
-- We need 'noinline' here, otherwise GHC will fuse allocL away and
-- unsound allocation can occur when multiple allocation done in serial!
allocL = GHC.noinline \(GHC.I# s) a -> Unsafe.toLinear \_ ->
  GHC.runRW# P.$ \st ->
    case GHC.newArray# s a st of
      (# _, arr #) -> unsafeCoerce# arr
{-# NOINLINE allocL #-} -- prevents runRW# from floating outwards
