{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}

module Data.Array.Mutable.Linear.ExtraSpec (
  test_allocL,
  test_fromListL,
) where

import Data.Alloc.Linearly.Token (linearly)
import qualified Data.Array.Mutable.Linear.Extra as LA
import Data.Unrestricted.Linear (unur)
import qualified Data.Vector as V
import qualified Prelude.Linear as PL
import qualified Test.Falsify.Generator as F
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as F
import Test.Tasty
import Test.Tasty.Falsify
import qualified Test.Tasty.Falsify as F

test_allocL :: TestTree
test_allocL =
  testGroup
    "allocL"
    [ testGroup
        "linearly (\\l -> freeze (allocL l n x)) = alloc n x freeze"
        [ testProperty "Int" do
            len <- gen $ F.int (F.between (0, 128))
            x <- gen $ F.int (F.withOrigin (-10, 10) 0)
            label "length" [classifyRangeBy 16 len]
            F.assert $
              P.eq
                .$ ("alloc", unur (LA.alloc len x LA.freeze))
                .$ ("allocL", unur (linearly \l -> LA.freeze (LA.allocL l len x)))
        ]
    ]

classifyRangeBy :: Int -> Int -> String
classifyRangeBy _ 0 = "0"
classifyRangeBy q n =
  let nDiv = (n - 1) `quot` q
   in show (nDiv * q + 1, (nDiv + 1) * q)

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
                .$ ("alloc", unur (linearly \l -> LA.freeze PL.$ LA.fromListL l xs))
                .$ ("replicate", V.fromList xs)
        , testProperty "Bool" do
            xs <- gen $ F.list (F.between (0, 128)) (F.bool True)
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.expect (V.fromList xs)
                .$ ("alloc", unur (linearly \l -> LA.freeze PL.$ LA.fromListL l xs))
        ]
    ]
