{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Array.Mutable.Unlifted.Linear.Extra (
  allocL,
  module Data.Array.Mutable.Unlifted.Linear,
) where

import Data.Alloc.Linearly.Token
import Data.Array.Mutable.Unlifted.Linear
import GHC.Exts (unsafeCoerce#)
import qualified GHC.Exts as GHC
import Prelude.Linear
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

allocL :: Linearly %1 -> Int -> a -> Array# a
{-# ANN allocL "HLint: ignore Avoid lambda" #-}
allocL = Unsafe.toLinear \_ (GHC.I# s) a ->
  GHC.runRW# P.$ \st ->
    case GHC.newArray# s a st of
      (# _, arr #) -> unsafeCoerce# arr
{-# NOINLINE allocL #-} -- prevents runRW# from floating outwards
