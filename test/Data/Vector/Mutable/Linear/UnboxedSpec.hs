{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Vector.Mutable.Linear.UnboxedSpec (
  test_constant,
  test_constantL,
  test_fromListL,
  test_fromList,
  test_empty,
  test_emptyL,
  test_fromArray,
) where

import Data.Alloc.Linearly.Token (linearly)
import qualified Data.Array.Mutable.Linear.Unboxed as LUA
import qualified Data.Functor.Linear as D
import Data.Unrestricted.Linear (unur)
import qualified Data.Vector.Mutable.Linear.Unboxed as LUV
import qualified Data.Vector.Unboxed as U
import Linear.Array.Extra.TestUtils
import Prelude.Linear (Ur (..))
import qualified Prelude.Linear as PL
import qualified Test.Falsify.Generator as F
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as F
import Test.Tasty
import Test.Tasty.Falsify
import qualified Test.Tasty.Falsify as F
import Test.Tasty.HUnit (testCase, (@?=))

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
                .$ ("constant", unur (LUV.constant len x LUV.freeze))
                .$ ("constantL", unur (linearly \l -> LUV.freeze (LUV.constantL l len x)))
        ]
    ]

test_constant :: TestTree
test_constant =
  testGroup
    "constant"
    [ testGroup
        "unur (constant n x freeze) = U.replicate n x"
        [ testProperty "Int" do
            len <- gen $ F.int (F.between (0, 128))
            x <- gen $ F.int (F.withOrigin (-10, 10) 0)
            label "length" [classifyRangeBy 16 len]
            F.assert $
              P.expect
                (U.replicate len x)
                .$ ("constant", unur (LUV.constant len x LUV.freeze))
        ]
    ]

test_emptyL :: TestTree
test_emptyL =
  testGroup
    "emptyL"
    [ testGroup
        "linearly (\\l -> freeze (emptyL l n x)) = empty n x freeze"
        [ testCase "Int" $
            unur (LUV.empty @Int LUV.freeze)
              @?= unur (linearly PL.$ LUV.freeze PL.. LUV.emptyL)
        , testCase "Double" $
            unur (LUV.empty @Double LUV.freeze)
              @?= unur (linearly PL.$ LUV.freeze PL.. LUV.emptyL)
        , testCase "Bool" $
            unur (LUV.empty @Bool LUV.freeze)
              @?= unur (linearly PL.$ LUV.freeze PL.. LUV.emptyL)
        ]
    ]

test_empty :: TestTree
test_empty =
  testGroup
    "empty"
    [ testGroup
        "unur (empty freeze) = U.empty"
        [ testCase "Int" $
            unur (LUV.empty @Int LUV.freeze) @?= U.empty
        , testCase "Double" $
            unur (LUV.empty @Double LUV.freeze) @?= U.empty
        , testCase "Bool" $
            unur (LUV.empty @Bool LUV.freeze) @?= U.empty
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
                .$ ("fromListL", unur (linearly \l -> LUV.freeze PL.$ LUV.fromListL l xs))
                .$ ("fromList", U.fromList xs)
        , testProperty "Bool" do
            xs <- gen $ F.list (F.between (0, 128)) (F.bool True)
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.expect (U.fromList xs)
                .$ ("fromListL", unur (linearly \l -> LUV.freeze PL.$ LUV.fromListL l xs))
        , testProperty "Double" do
            xs <- gen $ F.list (F.between (0, 128)) (doubleG 8)
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.expect (U.fromList xs)
                .$ ("fromListL", unur (linearly \l -> LUV.freeze PL.$ LUV.fromListL l xs))
        ]
    ]

test_fromList :: TestTree
test_fromList =
  testGroup
    "fromList"
    [ testGroup
        "unur (fromList xs freeze)) = U.fromList xs"
        [ testProperty "Int" $ checkFromList $ F.int (F.withOrigin (-10, 10) 0)
        , testProperty "Bool" $ checkFromList $ F.bool True
        , testProperty "Double" $ checkFromList $ doubleG 8
        ]
    ]

checkFromList :: (Show a, Eq a, U.Unbox a) => Gen a -> Property ()
checkFromList g = do
  xs <- gen $ F.list (F.between (0, 128)) g
  label "length" [classifyRangeBy 16 $ length xs]
  F.assert $
    P.expect (U.fromList xs)
      .$ ("fromList", unur (LUV.fromList xs LUV.freeze))

test_fromArray :: TestTree
test_fromArray =
  testGroup
    "fromArray"
    [ testGroup
        "is an array-homomorphism"
        [ checkFromArrayHomomorphism "Int" $ F.int $ F.between (-10, 10)
        , checkFromArrayHomomorphism "Bool" $ F.bool True
        , checkFromArrayHomomorphism "Double" $ F.bool True
        ]
    ]

checkFromArrayHomomorphism ::
  (Show a, Eq a, U.Unbox a) => String -> Gen a -> TestTree
checkFromArrayHomomorphism name g =
  testGroup
    name
    [ testProperty "freeze (set i x (fromArray xs)) = freeze (set i x xs)" do
        len <- F.gen $ F.integral $ F.between (1, 128)
        i <- F.gen $ F.int $ F.between (0, fromIntegral len - 1)
        x <- F.gen g
        xs <- F.gen $ F.list (F.between (len, len)) g
        label "length" [classifyRangeBy 16 $ length xs]
        collect "x == xs[i]" [show $ x == xs !! i]
        F.assert $
          P.eq
            .$ ( "array"
               , unur PL.$ linearly \l ->
                  LUA.freeze PL.$ LUA.set i x PL.$ LUA.fromListL l xs
               )
            .$ ( "vector"
               , unur PL.$ linearly \l ->
                  LUV.freeze PL.$ LUV.set i x PL.$ LUV.fromArray PL.$ LUA.fromListL l xs
               )
    , testProperty "freeze <$> get i (fromArray xs) = freeze <$> get i xs" do
        len <- F.gen $ F.integral $ F.between (1, 128)
        i <- F.gen $ F.int $ F.between (0, fromIntegral len - 1)
        xs <- F.gen $ F.list (F.between (len, len)) g
        label "length" [classifyRangeBy 16 $ length xs]
        F.assert $
          P.eq
            .$ ( "array"
               , unur PL.$ linearly \l ->
                  distribUr PL.$ D.fmap LUA.freeze PL.$ LUA.get i PL.$ LUA.fromListL l xs
               )
            .$ ( "vector"
               , unur PL.$ linearly \l ->
                  distribUr PL.$ D.fmap LUV.freeze PL.$ LUV.get i PL.$ LUV.fromArray PL.$ LUA.fromListL l xs
               )
    , testProperty "freeze <$> size (fromArray xs) = freeze <$> size xs" do
        len <- F.gen $ F.integral $ F.between (1, 128)
        xs <- F.gen $ F.list (F.between (len, len)) g
        label "length" [classifyRangeBy 16 $ length xs]
        F.assert $
          P.eq
            .$ ( "array"
               , unur PL.$ linearly \l ->
                  distribUr PL.$ D.fmap LUA.freeze PL.$ LUA.size PL.$ LUA.fromListL l xs
               )
            .$ ( "vector"
               , unur PL.$ linearly \l ->
                  distribUr PL.$ D.fmap LUV.freeze PL.$ LUV.size PL.$ LUV.fromArray PL.$ LUA.fromListL l xs
               )
    , testProperty "freeze (slice off ran (fromArray xs)) = freeze (snd' (unsafeSlice off ran xs))" do
        len <- F.gen $ F.integral $ F.between (0, 128)
        xs <- F.gen $ F.list (F.between (len, len)) g
        start <- F.gen $ F.int $ F.between (0, fromIntegral len)
        label
          "start %"
          [ if len == 0
              then "-"
              else classifyRangeBy 10 $ 100 * start `quot` fromIntegral len
          ]
        range <- F.gen $ F.int $ F.between (0, fromIntegral len - start)
        label
          "range %"
          [ if len == 0
              then "-"
              else classifyRangeBy 10 $ 100 * range `quot` fromIntegral len
          ]
        label "length" [classifyRangeBy 16 $ length xs]
        F.assert $
          P.eq
            .$ ( "array"
               , unur PL.$ linearly \l ->
                  LUA.freeze PL.$ snd' PL.$ LUA.unsafeSlice start range PL.$ LUA.fromListL l xs
               )
            .$ ( "vector"
               , unur PL.$ linearly \l ->
                  LUV.freeze PL.$ LUV.slice start range PL.$ LUV.fromArray PL.$ LUA.fromListL l xs
               )
    ]

distribUr :: (Ur a, Ur b) %1 -> Ur (a, b)
distribUr (Ur l, Ur r) = Ur (l, r)
