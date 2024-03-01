{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}

module Data.Array.Mutable.Linear.WitnessSpec (
  test_allocL,
  test_fromListL,
  test_serialAccess,
  test_doubleAlloc,
) where

import qualified Data.Array.Mutable.Linear as LA
import qualified Data.Array.Mutable.Linear.Witness as LA
import Data.Unrestricted.Linear (unur)
import qualified Data.Unrestricted.Linear as Ur
import qualified Data.Vector as V
import Linear.Token.Linearly (linearly)
import Linear.Token.Linearly.TestUtils
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
    [ testWithGens
        "linearly (\\l -> freeze (allocL l n x)) = alloc n x freeze"
        $ \g -> do
          len <- gen $ F.int (F.between (0, 128))
          x <- gen g
          label "length" [classifyRangeBy 16 len]
          F.assert $
            P.eq
              .$ ("alloc", unur (LA.alloc len x LA.freeze))
              .$ ("allocL", unur (linearly PL.$ LA.freeze PL.. LA.allocL len x))
    ]

test_fromListL :: TestTree
test_fromListL =
  testGroup
    "fromListL"
    [ testWithGens
        "unur (linearly \\l -> freeze (fromListL l xs)) = U.fromList xs"
        \g -> do
          xs <- gen $ F.list (F.between (0, 128)) g
          label "length" [classifyRangeBy 16 $ length xs]
          F.assert $
            P.eq
              .$ ("alloc", unur (linearly PL.$ LA.freeze PL.. LA.fromListL xs))
              .$ ("replicate", V.fromList xs)
    ]

test_doubleAlloc :: TestTree
test_doubleAlloc =
  testGroup
    "can be allocated inside linearly twice"
    [ testDoubleAlloc (F.int $ F.between (-10, 10)) (F.int $ F.between (-10, 10)) LA.fromListL LA.fromListL (Ur.lift V.toList PL.. LA.freeze) (Ur.lift V.toList PL.. LA.freeze)
    , testDoubleAlloc (F.int $ F.between (-10, 10)) (F.bool True) LA.fromListL LA.fromListL (Ur.lift V.toList PL.. LA.freeze) (Ur.lift V.toList PL.. LA.freeze)
    , testDoubleAlloc (F.int $ F.between (-10, 10)) (doubleG 8) LA.fromListL LA.fromListL (Ur.lift V.toList PL.. LA.freeze) (Ur.lift V.toList PL.. LA.freeze)
    , testDoubleAlloc (F.bool True) (F.int $ F.between (-10, 10)) LA.fromListL LA.fromListL (Ur.lift V.toList PL.. LA.freeze) (Ur.lift V.toList PL.. LA.freeze)
    , testDoubleAlloc (F.bool True) (F.bool True) LA.fromListL LA.fromListL (Ur.lift V.toList PL.. LA.freeze) (Ur.lift V.toList PL.. LA.freeze)
    , testDoubleAlloc (F.bool True) (doubleG 8) LA.fromListL LA.fromListL (Ur.lift V.toList PL.. LA.freeze) (Ur.lift V.toList PL.. LA.freeze)
    , testDoubleAlloc (doubleG 8) (F.int $ F.between (-10, 10)) LA.fromListL LA.fromListL (Ur.lift V.toList PL.. LA.freeze) (Ur.lift V.toList PL.. LA.freeze)
    , testDoubleAlloc (doubleG 8) (F.bool True) LA.fromListL LA.fromListL (Ur.lift V.toList PL.. LA.freeze) (Ur.lift V.toList PL.. LA.freeze)
    , testDoubleAlloc (doubleG 8) (doubleG 8) LA.fromListL LA.fromListL (Ur.lift V.toList PL.. LA.freeze) (Ur.lift V.toList PL.. LA.freeze)
    ]

test_serialAccess :: TestTree
test_serialAccess =
  testGroup
    "Serial Updates has the same meaning with vectors"
    [ testProperty "Int" $
        checkSerialUpdateSemantics
          (F.int $ F.between (-10, 10))
          LA.fromListL
          LA.freeze
    , testProperty "Bool" $
        checkSerialUpdateSemantics
          (F.bool True)
          LA.fromListL
          LA.freeze
    , testProperty "Double" $
        checkSerialUpdateSemantics
          (doubleG 8)
          LA.fromListL
          LA.freeze
    ]
