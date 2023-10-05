{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Array.Mutable.Linear.Witness (
  allocL,
  fromListL,
) where

import Data.Array.Mutable.Linear
import Data.Array.Mutable.Linear.Internal
import qualified Data.Array.Mutable.Unlifted.Linear.Witness as Unlifted
import GHC.Stack (HasCallStack)
import Linear.Witness.Token
import Prelude.Linear
import qualified Prelude as P

allocL :: Int -> a -> Linearly %1 -> Array a
allocL i a l = Array (Unlifted.allocL i a l)

fromListL :: (HasCallStack) => [a] -> Linearly %1 -> Array a
fromListL (list :: [a]) =
  insert
    . allocL
      (P.length list)
      (error "invariant violation: unintialized array position")
  where
    insert :: Array a %1 -> Array a
    insert = doWrites (P.zip list [0 ..])

    doWrites :: [(a, Int)] -> Array a %1 -> Array a
    doWrites [] arr = arr
    doWrites ((a, ix) : xs) arr = doWrites xs (unsafeSet ix a arr)
