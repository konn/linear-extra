{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.HashMap.Mutable.Linear.Witness (
  emptyL,
  fromListL,
) where

import qualified Data.Array.Mutable.Linear.Witness as Array
import Data.HashMap.Mutable.Linear
import Data.HashMap.Mutable.Linear.Internal
import Linear.Witness.Token
import Prelude.Linear hiding (lookup)
import Prelude hiding (lookup, mempty, seq, ($), (.))
import qualified Prelude as P

emptyL :: (Keyed k) => Int -> Linearly %1 -> HashMap k v
emptyL size l =
  let cap = P.max 1 size
   in HashMap 0 cap (Array.allocL cap Nothing l)

fromListL :: (Keyed k) => [(k, v)] -> Linearly %1 -> HashMap k v
fromListL (xs :: [(k, v)]) =
  let cap =
        P.max
          1
          (ceiling @Float @Int (fromIntegral (Prelude.length xs) / constMaxLoadFactor))
   in insertAll xs . HashMap 0 cap . Array.allocL cap Nothing
