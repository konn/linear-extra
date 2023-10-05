{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}

module Data.Set.Mutable.Linear.WitnessSpec (test_doubleAlloc) where

import qualified Data.Bifunctor as Bi
import qualified Data.Set as Set
import qualified Data.Set.Mutable.Linear as LSet
import qualified Data.Set.Mutable.Linear.Witness as LSet
import Data.Unrestricted.Linear (unur)
import Linear.Witness.Token (besides, linearly)
import Linear.Witness.Token.TestUtils
import Prelude.Linear ((&))
import qualified Test.Falsify.Generator as F
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as F
import Test.Tasty
import Test.Tasty.Falsify
import qualified Test.Tasty.Falsify as F

test_doubleAlloc :: TestTree
test_doubleAlloc =
  testGroup
    "can be allocated inside linearly twice"
    [ testProperty "(Int, Double)" do
        let intG = F.int $ F.between (-100, 100)
        xs <- F.gen $ F.list (F.between (0, 16)) intG
        ys <- F.gen $ F.list (F.between (0, 16)) $ doubleG 8
        F.assert $
          P.expect (Set.fromList xs, Set.fromList ys)
            .$ ( "actual"
               , Bi.bimap Set.fromList Set.fromList $
                  unur
                    ( linearly \l ->
                        besides l (LSet.fromListL xs) & \(xs', l') ->
                          LSet.fromListL ys l' & \ys' ->
                            distribUr (LSet.toList xs', LSet.toList ys')
                    )
               )
        pure ()
    ]