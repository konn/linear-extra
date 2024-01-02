{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Set.Mutable.Linear.Witness (
  module Data.Set.Mutable.Linear,
  emptyL,
  fromListL,
) where

import qualified Data.HashMap.Mutable.Linear.Witness as LHM
import Data.Set.Mutable.Linear
import Data.Set.Mutable.Linear.Internal
import Linear.Token.Linearly
import Prelude.Linear hiding (lookup)
import qualified Prelude.Linear as P
import Prelude hiding (lookup, mempty, seq, ($), (.))

emptyL :: (Keyed a) => Int -> Linearly %1 -> Set a
emptyL cap = Set . LHM.emptyL cap

fromListL :: (Keyed a) => [a] -> Linearly %1 -> Set a
fromListL as = Set . LHM.fromListL (P.map (,()) as)