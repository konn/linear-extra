{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Vector.Mutable.Linear.Extra (
  emptyL,
  constantL,
  fromListL,
  module Data.Vector.Mutable.Linear,
) where

import Data.Alloc.Linearly.Token
import qualified Data.Array.Mutable.Linear.Extra as Array
import Data.Vector.Mutable.Linear
import Data.Vector.Mutable.Linear.Internal
import Prelude.Linear

emptyL :: Linearly %1 -> Vector a
emptyL l = Vec 0 (Array.fromListL l [])

constantL :: Linearly %1 -> Int -> a -> Vector a
constantL l n a = Vec n (Array.allocL l n a)

fromListL :: Linearly %1 -> [a] -> Vector a
fromListL l as = fromArray (Array.fromListL l as)
