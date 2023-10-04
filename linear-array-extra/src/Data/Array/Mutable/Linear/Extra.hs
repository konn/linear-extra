{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Array.Mutable.Linear.Extra (
  module Data.Array.Mutable.Linear,
  allocL,
  fromListL,
) where

import Data.Alloc.Linearly.Token
import Data.Array.Mutable.Linear
import Data.Array.Mutable.Linear.Internal
import qualified Data.Array.Mutable.Unlifted.Linear.Extra as Unlifted
import GHC.Stack (HasCallStack)
import Prelude.Linear
import qualified Prelude as P

allocL :: Linearly %1 -> Int -> a -> Array a
allocL l i a = Array (Unlifted.allocL l i a)

fromListL ::
  HasCallStack =>
  Linearly %1 ->
  [a] ->
  Array a
fromListL l (list :: [a]) =
  insert
    ( allocL
        l
        (P.length list)
        (error "invariant violation: unintialized array position")
    )
  where
    insert :: Array a %1 -> Array a
    insert = doWrites (P.zip list [0 ..])

    doWrites :: [(a, Int)] -> Array a %1 -> Array a
    doWrites [] arr = arr
    doWrites ((a, ix) : xs) arr = doWrites xs (unsafeSet ix a arr)
