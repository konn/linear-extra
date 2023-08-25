{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Alloc.Linearly.Token.Internal (Linearly (..), unsafeLinearly) where

import Data.Primitive (newPinnedPrimArray)
import Data.Primitive.PrimArray (MutablePrimArray, readPrimArray, writePrimArray)
import Data.Unrestricted.Linear
import GHC.Exts
import GHC.IO (unIO)
import Generics.Linear.TH (deriveGeneric)
import Prelude.Linear
import System.IO.Unsafe (unsafePerformIO)
import qualified Prelude as P

newtype Linearly = Linearly Int

seed :: MutablePrimArray RealWorld Int
{-# NOINLINE seed #-}
seed = unsafePerformIO do
  pa <- newPinnedPrimArray 0
  writePrimArray pa 0 1
  P.pure pa

unsafeLinearly :: (Linearly %1 -> a) %1 -> a
{-# NOINLINE unsafeLinearly #-}
unsafeLinearly k = case runRW# P.$ unIO $ readPrimArray seed 0 of
  (# s, i #) ->
    case unIO (writePrimArray seed 0 (1 - i)) s of
      (# _, () #) -> k (Linearly i)

deriveGeneric ''Linearly

deriving newtype instance Consumable Linearly

deriving newtype instance Dupable Linearly

deriving newtype instance Movable Linearly
