{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Vector.Mutable.Linear.ExtraSpec (test_constantL, test_fromListL, test_emptyL) where

import Data.Alloc.Linearly.Token (linearly)
import Data.Unrestricted.Linear (unur)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable.Linear.Extra as LV
import Linear.Array.Extra.TestUtils
import qualified Prelude.Linear as PL
import qualified Test.Falsify.Generator as F
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as F
import Test.Tasty
import Test.Tasty.Falsify
import qualified Test.Tasty.Falsify as F
import Test.Tasty.HUnit (assertEqual, testCase)

test_constantL :: TestTree
test_constantL =
  testGroup
    "constantL"
    [ testGroup
        "linearly (\\l -> freeze (constantL l n x)) = constant n x freeze"
        [ testProperty "Int" do
            len <- gen $ F.int (F.between (0, 128))
            x <- gen $ F.int (F.withOrigin (-10, 10) 0)
            label "length" [classifyRangeBy 16 len]
            F.assert $
              P.eq
                .$ ("constant", unur (LV.constant len x LV.freeze))
                .$ ("constantL", unur (linearly \l -> LV.freeze (LV.constantL l len x)))
        ]
    ]

test_emptyL :: TestTree
test_emptyL =
  testGroup
    "emptyL"
    [ testGroup
        "linearly (\\l -> freeze (emptyL l n x)) = empty n x freeze"
        [ testCase "Int" do
            assertEqual
              ""
              (unur (LV.empty @Int LV.freeze))
              (unur (linearly PL.$ LV.freeze PL.. LV.emptyL))
        ]
    ]

test_fromListL :: TestTree
test_fromListL =
  testGroup
    "fromListL"
    [ testGroup
        "unur (linearly \\l -> freeze (fromListL l xs)) = U.fromList xs"
        [ testProperty "Int" do
            xs <- gen $ F.list (F.between (0, 128)) (F.int (F.withOrigin (-10, 10) 0))
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.eq
                .$ ("fromListL", unur (linearly \l -> LV.freeze PL.$ LV.fromListL l xs))
                .$ ("fromList", V.fromList xs)
        , testProperty "Bool" do
            xs <- gen $ F.list (F.between (0, 128)) (F.bool True)
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.expect (V.fromList xs)
                .$ ("fromListL", unur (linearly \l -> LV.freeze PL.$ LV.fromListL l xs))
        ]
    ]
