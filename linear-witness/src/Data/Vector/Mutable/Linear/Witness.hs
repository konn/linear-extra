{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Vector.Mutable.Linear.Witness (
  emptyL,
  constantL,
  fromListL,
) where

import qualified Data.Array.Mutable.Linear.Witness as Array
import Data.Vector.Mutable.Linear
import Data.Vector.Mutable.Linear.Internal
import Linear.Token.Linearly
import Prelude.Linear

emptyL :: Linearly %1 -> Vector a
emptyL = Vec 0 . Array.fromListL []

constantL :: Int -> a -> Linearly %1 -> Vector a
constantL n a = Vec n . Array.allocL n a

fromListL :: [a] -> Linearly %1 -> Vector a
fromListL as = fromArray . Array.fromListL as
