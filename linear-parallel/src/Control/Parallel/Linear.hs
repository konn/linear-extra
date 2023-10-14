{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

module Control.Parallel.Linear (
  pseq,
  lpseq,
  par,
  lpar,
) where

import qualified GHC.Conc as GHC
import GHC.Exts
import qualified GHC.Exts as GHC
import Prelude.Linear
import qualified Unsafe.Linear as Unsafe

-- | A linear variant of 'GHC.Conc.par', but returns both arguments to avoid consuming the first.
pseq :: a %1 -> b %1 -> (a, b)
{-# INLINE pseq #-}
pseq = Unsafe.toLinear2 \a b -> a `GHC.pseq` (a, b)

{- | A linear variant of 'GHC.Conc.pseq' which consumes the first argument strictly and returns y /lazily/.
This can be thought as the variant of 'lseq' with the second argument lazy.
-}
lpseq :: (Consumable a) => a %1 -> b %1 -> b
{-# INLINE lpseq #-}
lpseq = Unsafe.toLinear2 \x y -> x `lseq` lazy y

{- |
A linear variant of 'GHC.Conc.par', but returns both arguments to avoid consuming the first.
-}
par :: a %1 -> b %1 -> (a, b)
{-# INLINE par #-}
{- HLint ignore par -}
par = Unsafe.toLinear2 \x y ->
  case GHC.par# x of
    _ -> (x, lazy y)

{- |
A linear variant of 'GHC.Conc.par' which consumes the first argument parallely.
In otherwords: this is semantically equivalent to @x `lseq` y@, but consumes @x@ parallelly.
-}
lpar :: (Consumable a) => a %1 -> b %1 -> b
{-# INLINE lpar #-}
lpar = Unsafe.toLinear2 \x y ->
  consume x `GHC.par` y
