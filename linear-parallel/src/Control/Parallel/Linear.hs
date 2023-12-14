{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

module Control.Parallel.Linear (
  pseq,
  lpseq,
  par,
  lpar,
) where

import qualified Control.Functor.Linear as C
import Control.Parallel.Strategy.Linear
import qualified GHC.Conc as GHC
import GHC.Exts
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

N.B.: We need to seq both arguments to ensure the both computation finishes when @par@ returned.
-}
par :: a %1 -> b %1 -> (a, b)
{-# NOINLINE par #-}
par a b = runEval C.do
  a <- rpar a
  b <- rpar b
  a <- rseq a
  b <- rseq b
  C.pure (a, b)

{- |
A linear variant of 'GHC.Conc.par' which consumes the first argument parallely.
In otherwords: this is semantically equivalent to @x `lseq` y@, but consumes @x@ parallelly.
-}
lpar :: (Consumable a) => a %1 -> b %1 -> b
{-# INLINE lpar #-}
lpar = Unsafe.toLinear2 \x y ->
  consume x `GHC.par` y
