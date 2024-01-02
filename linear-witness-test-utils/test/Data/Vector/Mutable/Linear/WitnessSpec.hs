{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Data.Vector.Mutable.Linear.WitnessSpec (
  test_constant,
  test_constantL,
  test_fromListL,
  test_fromList,
  test_empty,
  test_emptyL,
  test_doubleAlloc,
  test_serialAccess,
  test_push,
  test_pop,
  test_mapMaybe,
  test_filter,
  test_slice,
  test_doubleAlloc_empty,
) where

import qualified Data.Functor.Linear as D
import Data.Unrestricted.Linear (unur)
import qualified Data.Unrestricted.Linear as Ur
import qualified Data.Vector as V
import qualified Data.Vector.Mutable.Linear as LV
import qualified Data.Vector.Mutable.Linear.Witness as LV
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
import Test.Tasty.HUnit (testCase, (@?=))

test_constantL :: TestTree
test_constantL =
  testGroup
    "constantL"
    [ testWithGens
        "linearly (\\l -> freeze (constantL l n x)) = constant n x freeze"
        \g -> do
          len <- gen $ F.int (F.between (0, 128))
          x <- gen g
          label "length" [classifyRangeBy 16 len]
          F.assert $
            P.eq
              .$ ("constant", unur (LV.constant len x LV.freeze))
              .$ ("constantL", unur (linearly PL.$ LV.freeze PL.. LV.constantL len x))
    ]

test_constant :: TestTree
test_constant =
  testGroup
    "constant"
    [ testWithGens
        "unur (constant n x freeze) = V.replicate n x"
        \g -> do
          len <- gen $ F.int (F.between (0, 128))
          x <- gen g
          label "length" [classifyRangeBy 16 len]
          F.assert $
            P.expect
              (V.replicate len x)
              .$ ("constant", unur (LV.constant len x LV.freeze))
    ]

test_emptyL :: TestTree
test_emptyL =
  testGroup
    "emptyL"
    [ testGroup
        "linearly (\\l -> freeze (emptyL l n x)) = empty n x freeze"
        [ testCase "Int" $
            unur (LV.empty @Int LV.freeze)
              @?= unur (linearly PL.$ LV.freeze PL.. LV.emptyL)
        , testCase "Double" $
            unur (LV.empty @Double LV.freeze)
              @?= unur (linearly PL.$ LV.freeze PL.. LV.emptyL)
        , testCase "Bool" $
            unur (LV.empty @Bool LV.freeze)
              @?= unur (linearly PL.$ LV.freeze PL.. LV.emptyL)
        ]
    ]

test_empty :: TestTree
test_empty =
  testGroup
    "empty"
    [ testGroup
        "unur (empty freeze) = V.empty"
        [ testCase "Int" $
            unur (LV.empty @Int LV.freeze) @?= V.empty
        , testCase "Double" $
            unur (LV.empty @Double LV.freeze) @?= V.empty
        , testCase "Bool" $
            unur (LV.empty @Bool LV.freeze) @?= V.empty
        ]
    ]

test_fromListL :: TestTree
test_fromListL =
  testGroup
    "fromListL"
    [ testWithGens
        "unur (linearly \\l -> freeze (fromListL l xs)) = V.fromList xs"
        \g -> do
          (_len, xs) <- genLenList g
          F.assert $
            P.eq
              .$ ("fromListL", unur (linearly PL.$ \l -> LV.freeze PL.$ LV.fromListL xs l))
              .$ ("fromList", V.fromList xs)
    ]

test_fromList :: TestTree
test_fromList =
  testGroup
    "fromList"
    [ testWithGens
        "unur (fromList xs freeze)) = V.fromList xs"
        checkFromList
    ]

checkFromList :: (Show a, Eq a) => Gen a -> Property ()
checkFromList g = do
  xs <- gen $ F.list (F.between (0, 128)) g
  label "length" [classifyRangeBy 16 $ length xs]
  F.assert $
    P.expect (V.fromList xs)
      .$ ("fromList", unur (LV.fromList xs LV.freeze))

test_serialAccess :: TestTree
test_serialAccess =
  testGroup
    "Serial Updates has the same meaning with vectors"
    [ testProperty "Int" $
        checkSerialUpdateSemantics
          (F.int $ F.between (-10, 10))
          LV.fromListL
          LV.freeze
    , testProperty "Bool" $
        checkSerialUpdateSemantics
          (F.bool True)
          LV.fromListL
          LV.freeze
    , testProperty "Double" $
        checkSerialUpdateSemantics
          (doubleG 8)
          LV.fromListL
          LV.freeze
    ]

test_push :: TestTree
test_push =
  testGroup
    "push"
    [ testWithGens "freeze . push = snoc" checkPushSnoc
    , testWithGens "freeze (pop (push x xs)) = (Just x, xs)" \g -> do
        (_len, xs) <- genLenList g
        x <- F.gen g
        F.assert $
          P.expect (Just x, V.fromList xs)
            .$ ( "actual"
               , unur PL.$ linearly \l ->
                  distribUr
                    ( LV.freeze
                        D.<$> LV.pop (LV.push x (LV.fromListL xs l))
                    )
               )
    ]

test_pop :: TestTree
test_pop =
  testGroup
    "push"
    [ testWithGens "pop = unsnoc" checkPushSnoc
    , testWithGens "freeze (pop (push x xs)) = (Just x, xs)" \g -> do
        (_len, xs) <- genLenList g
        let uv = V.fromList xs
        F.assert $
          P.expect
            ( case V.unsnoc uv of
                Just (xs', x) -> (Just x, xs')
                Nothing -> (Nothing, uv)
            )
            .$ ( "actual"
               , unur PL.$ linearly \l ->
                  distribUr
                    ( LV.freeze
                        D.<$> LV.pop (LV.fromListL xs l)
                    )
               )
    ]

test_mapMaybe :: TestTree
test_mapMaybe =
  testGroup
    "mapMaybe"
    [ testGroup
        "freeze . mapMaybe f = V.mapMaybe f"
        [ testWithGens "-> Int" $ checkMapMaybe $ F.int $ F.withOrigin (-10, 10) 0
        , testWithGens "-> Bool" $ checkMapMaybe $ F.bool True
        , testWithGens "-> Double" $ checkMapMaybe $ doubleG 8
        ]
    ]

test_filter :: TestTree
test_filter =
  testGroup
    "filter"
    [testWithGens "freeze . filter p = V.filter p" checkFilter]

checkMapMaybe ::
  ( F.Function a
  , Show a
  , Show b
  , Eq b
  ) =>
  Gen b ->
  Gen a ->
  Property' String ()
checkMapMaybe tgt g = do
  (len, xs) <- genLenList g
  F.Fn f <-
    F.gen $
      F.fun $
        pure Nothing `F.choose` (Just <$> tgt)
  let resl = V.mapMaybe f $ V.fromList xs
  label
    "% length of reduced vector"
    [classifyPercent (V.length resl) $ fromIntegral len]
  F.assert $
    P.expect resl
      .$ ( "actual"
         , unur PL.$
            linearly \l ->
              LV.freeze PL.$
                LV.mapMaybe (LV.fromListL xs l) f
         )

checkFilter ::
  ( F.Function a
  , Show a
  , Eq a
  ) =>
  Gen a ->
  Property' String ()
checkFilter g = do
  (len, xs) <- genLenList g
  F.Fn p <- F.gen $ F.fun $ F.bool False
  let resl = V.filter p $ V.fromList xs
  label
    "% length of reduced vector"
    [classifyPercent (V.length resl) $ fromIntegral len]
  F.assert $
    P.expect resl
      .$ ( "actual"
         , unur PL.$
            linearly \l ->
              LV.freeze PL.$
                LV.filter (LV.fromListL xs l) p
         )

checkPushSnoc :: (Eq a, Show a) => Gen a -> Property' String ()
checkPushSnoc g = do
  (_l, xs) <- genLenList g
  x <- F.gen g
  F.assert $
    P.expect (V.fromList xs `V.snoc` x)
      .$ ( "actual"
         , unur PL.$
            linearly PL.$ \l ->
              LV.freeze PL.$
                LV.push x PL.$
                  LV.fromListL xs l
         )

test_slice :: TestTree
test_slice =
  testGroup
    "slice"
    [ testWithGens "freeze . slice off len xs = slice off len " \g -> do
        (len, xs) <- genLenList g
        Slice {..} <- genSlice len
        let sliced = V.slice offset range $ V.fromList xs
        F.assert $
          P.expect sliced
            .$ ( "actual"
               , unur PL.$
                  linearly PL.$
                    LV.freeze
                      PL.. LV.slice offset range
                      PL.. LV.fromListL xs
               )
    ]

test_doubleAlloc :: TestTree
test_doubleAlloc =
  testGroup
    "can be allocated inside linearly twice"
    [ testDoubleAlloc (F.int $ F.between (-10, 10)) (F.int $ F.between (-10, 10)) LV.fromListL LV.fromListL (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAlloc (F.int $ F.between (-10, 10)) (F.bool True) LV.fromListL LV.fromListL (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAlloc (F.int $ F.between (-10, 10)) (doubleG 8) LV.fromListL LV.fromListL (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAlloc (F.bool True) (F.int $ F.between (-10, 10)) LV.fromListL LV.fromListL (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAlloc (F.bool True) (F.bool True) LV.fromListL LV.fromListL (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAlloc (F.bool True) (doubleG 8) LV.fromListL LV.fromListL (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAlloc (doubleG 8) (F.int $ F.between (-10, 10)) LV.fromListL LV.fromListL (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAlloc (doubleG 8) (F.bool True) LV.fromListL LV.fromListL (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAlloc (doubleG 8) (doubleG 8) LV.fromListL LV.fromListL (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    ]

test_doubleAlloc_empty :: TestTree
test_doubleAlloc_empty =
  testGroup
    "can be allocated inside linearly twice (empty)"
    [ testDoubleAllocSnoc (F.int $ F.between (-10, 10)) (F.int $ F.between (-10, 10)) LV.emptyL LV.emptyL LV.push LV.push (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAllocSnoc (F.int $ F.between (-10, 10)) (F.bool True) LV.emptyL LV.emptyL LV.push LV.push (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAllocSnoc (F.int $ F.between (-10, 10)) (doubleG 8) LV.emptyL LV.emptyL LV.push LV.push (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAllocSnoc (F.bool True) (F.int $ F.between (-10, 10)) LV.emptyL LV.emptyL LV.push LV.push (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAllocSnoc (F.bool True) (F.bool True) LV.emptyL LV.emptyL LV.push LV.push (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAllocSnoc (F.bool True) (doubleG 8) LV.emptyL LV.emptyL LV.push LV.push (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAllocSnoc (doubleG 8) (F.int $ F.between (-10, 10)) LV.emptyL LV.emptyL LV.push LV.push (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAllocSnoc (doubleG 8) (F.bool True) LV.emptyL LV.emptyL LV.push LV.push (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    , testDoubleAllocSnoc (doubleG 8) (doubleG 8) LV.emptyL LV.emptyL LV.push LV.push (Ur.lift V.toList PL.. LV.freeze) (Ur.lift V.toList PL.. LV.freeze)
    ]
