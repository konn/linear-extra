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
import Prelude.Linear
import qualified Unsafe.Linear as Unsafe

data ArrayC a = ArrayC (Array# a)

allocL :: Linearly %1 -> Int -> a -> Array# a
{-# ANN allocL "HLint: ignore Avoid lambda" #-}
allocL l s x =
  consume l & \() ->
    alloc s x (Unsafe.toLinear (\arr# -> Ur (ArrayC arr#))) & \(Ur (ArrayC arr)) -> arr
{-# NOINLINE allocL #-} -- prevents runRW# from floating outwards
