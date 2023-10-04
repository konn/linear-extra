{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Data.Vector.Mutable.Linear.UnboxedSpec (
  test_constant,
  test_constantL,
  test_fromListL,
  test_fromList,
  test_empty,
  test_emptyL,
  test_fromArray,
  test_doubleAlloc,
  test_serialAccess,
  test_push,
  test_pop,
  test_mapMaybe,
  test_filter,
  test_mapSame,
  test_slice,
) where

import qualified Data.Array.Mutable.Linear.Unboxed as LUA
import qualified Data.Functor.Linear as D
import Data.Unrestricted.Linear (unur)
import qualified Data.Unrestricted.Linear as Ur
import qualified Data.Vector.Mutable.Linear.Unboxed as LUV
import qualified Data.Vector.Unboxed as U
import Linear.Array.Extra.TestUtils
import Linear.Witness.Token (linearly)
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
              .$ ("constant", unur (LUV.constant len x LUV.freeze))
              .$ ("constantL", unur (linearly PL.$ LUV.freeze PL.. LUV.constantL len x))
    ]

test_constant :: TestTree
test_constant =
  testGroup
    "constant"
    [ testWithGens
        "unur (constant n x freeze) = U.replicate n x"
        \g -> do
          len <- gen $ F.int (F.between (0, 128))
          x <- gen g
          label "length" [classifyRangeBy 16 len]
          F.assert $
            P.expect
              (U.replicate len x)
              .$ ("constant", unur (LUV.constant len x LUV.freeze))
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
    [ testWithGens
        "unur (linearly \\l -> freeze (fromListL l xs)) = U.fromList xs"
        \g -> do
          (_len, xs) <- genLenList g
          F.assert $
            P.eq
              .$ ("fromListL", unur (linearly PL.$ \l -> LUV.freeze PL.$ LUV.fromListL xs l))
              .$ ("fromList", U.fromList xs)
    ]

test_fromList :: TestTree
test_fromList =
  testGroup
    "fromList"
    [ testWithGens
        "unur (fromList xs freeze)) = U.fromList xs"
        checkFromList
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
        [ testWithGens "freeze (set i x (fromArray xs)) = freeze (set i x xs)" \g -> do
            len <- F.gen $ F.integral $ F.between (1, 128)
            i <- F.gen $ F.int $ F.between (0, fromIntegral len - 1)
            x <- F.gen g
            xs <- F.gen $ F.list (F.between (len, len)) g
            label "length" [classifyRangeBy 16 $ length xs]
            collect "x == xs[i]" [show $ x == xs !! i]
            F.assert $
              P.eq
                .$ ( "array"
                   , unur PL.$ linearly PL.$ \l ->
                      LUA.freeze PL.$ LUA.set i x PL.$ LUA.fromListL xs l
                   )
                .$ ( "vector"
                   , unur PL.$ linearly PL.$ \l ->
                      LUV.freeze PL.$ LUV.set i x PL.$ LUV.fromArray PL.$ LUA.fromListL xs l
                   )
        , testWithGens "freeze <$> get i (fromArray xs) = freeze <$> get i xs" \g -> do
            len <- F.gen $ F.integral $ F.between (1, 128)
            i <- F.gen $ F.int $ F.between (0, fromIntegral len - 1)
            xs <- F.gen $ F.list (F.between (len, len)) g
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.eq
                .$ ( "array"
                   , unur PL.$ linearly PL.$ \l ->
                      distribUr PL.$ D.fmap LUA.freeze PL.$ LUA.get i PL.$ LUA.fromListL xs l
                   )
                .$ ( "vector"
                   , unur PL.$ linearly PL.$ \l ->
                      distribUr PL.$ D.fmap LUV.freeze PL.$ LUV.get i PL.$ LUV.fromArray PL.$ LUA.fromListL xs l
                   )
        , testWithGens "freeze <$> size (fromArray xs) = freeze <$> size xs" \g -> do
            len <- F.gen $ F.integral $ F.between (1, 128)
            xs <- F.gen $ F.list (F.between (len, len)) g
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.eq
                .$ ( "array"
                   , unur PL.$ linearly PL.$ \l ->
                      distribUr PL.$ D.fmap LUA.freeze PL.$ LUA.size PL.$ LUA.fromListL xs l
                   )
                .$ ( "vector"
                   , unur PL.$ linearly PL.$ \l ->
                      distribUr PL.$ D.fmap LUV.freeze PL.$ LUV.size PL.$ LUV.fromArray PL.$ LUA.fromListL xs l
                   )
        , testWithGens "freeze (slice off ran (fromArray xs)) = freeze (snd' (unsafeSlice off ran xs))" \g -> do
            len <- F.gen $ F.integral $ F.between (0, 128)
            xs <- F.gen $ F.list (F.between (len, len)) g
            start <- F.gen $ F.int $ F.between (0, fromIntegral len)
            label
              "start %"
              [ classifyPercent start $ fromIntegral len
              ]
            range <- F.gen $ F.int $ F.between (0, fromIntegral len - start)
            label
              "range %"
              [ classifyPercent range $ fromIntegral len
              ]
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.eq
                .$ ( "array"
                   , unur PL.$ linearly PL.$ \l ->
                      LUA.freeze PL.$ snd' PL.$ LUA.unsafeSlice start range PL.$ LUA.fromListL xs l
                   )
                .$ ( "vector"
                   , unur PL.$ linearly PL.$ \l ->
                      LUV.freeze PL.$ LUV.slice start range PL.$ LUV.fromArray PL.$ LUA.fromListL xs l
                   )
        , testGroup
            "freeze (map f (fromArray xs)) = freeze (map f xs)"
            [ testWithGens "-> Int" \g -> do
                len <- F.gen $ F.integral $ F.between (0, 128)
                xs <- F.gen $ F.list (F.between (len, len)) g
                Fn f <- F.gen $ F.fun $ F.int (F.between (-10, 10))
                label "length" [classifyRangeBy 16 $ length xs]
                F.assert $
                  P.eq
                    .$ ( "array"
                       , unur PL.$ linearly PL.$ \l ->
                          LUA.freeze PL.$ LUA.map f PL.$ LUA.fromListL xs l
                       )
                    .$ ( "vector"
                       , unur PL.$ linearly PL.$ \l ->
                          LUV.freeze PL.$ PL.flip LUV.map f PL.$ LUV.fromArray PL.$ LUA.fromListL xs l
                       )
            , testWithGens "-> Bool" \g -> do
                len <- F.gen $ F.integral $ F.between (0, 128)
                xs <- F.gen $ F.list (F.between (len, len)) g
                Fn f <- F.gen $ F.fun $ F.bool True
                label "length" [classifyRangeBy 16 $ length xs]
                F.assert $
                  P.eq
                    .$ ( "array"
                       , unur PL.$ linearly PL.$ \l ->
                          LUA.freeze PL.$ LUA.map f PL.$ LUA.fromListL xs l
                       )
                    .$ ( "vector"
                       , unur PL.$ linearly PL.$ \l ->
                          LUV.freeze PL.$ PL.flip LUV.map f PL.$ LUV.fromArray PL.$ LUA.fromListL xs l
                       )
            ]
        , testWithGens
            "freeze (mapSame f (fromArray xs)) = freeze (mapSame f xs)"
            \g -> do
              len <- F.gen $ F.integral $ F.between (0, 128)
              xs <- F.gen $ F.list (F.between (len, len)) g
              Fn f <- F.gen $ F.fun g
              label "length" [classifyRangeBy 16 $ length xs]
              F.assert $
                P.eq
                  .$ ( "array"
                     , unur PL.$ linearly PL.$ \l ->
                        LUA.freeze PL.$ LUA.mapSame f PL.$ LUA.fromListL xs l
                     )
                  .$ ( "vector"
                     , unur PL.$ linearly PL.$ \l ->
                        LUV.freeze PL.$ PL.flip LUV.mapSame f PL.$ LUV.fromArray PL.$ LUA.fromListL xs l
                     )
        ]
    ]

test_serialAccess :: TestTree
test_serialAccess =
  testGroup
    "Serial Updates has the same meaning with vectors"
    [ testProperty "Int" $
        checkSerialUpdateSemantics
          (F.int $ F.between (-10, 10))
          LUV.fromListL
          LUV.freeze
    , testProperty "Bool" $
        checkSerialUpdateSemantics
          (F.bool True)
          LUV.fromListL
          LUV.freeze
    , testProperty "Double" $
        checkSerialUpdateSemantics
          (doubleG 8)
          LUV.fromListL
          LUV.freeze
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
          P.expect (Just x, U.fromList xs)
            .$ ( "actual"
               , unur PL.$ linearly \l ->
                  distribUr
                    ( LUV.freeze
                        D.<$> LUV.pop (LUV.push x (LUV.fromListL xs l))
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
        let uv = U.fromList xs
        F.assert $
          P.expect
            ( case U.unsnoc uv of
                Just (xs', x) -> (Just x, xs')
                Nothing -> (Nothing, uv)
            )
            .$ ( "actual"
               , unur PL.$ linearly \l ->
                  distribUr
                    ( LUV.freeze
                        D.<$> LUV.pop (LUV.fromListL xs l)
                    )
               )
    ]

test_mapMaybe :: TestTree
test_mapMaybe =
  testGroup
    "mapMaybe"
    [ testGroup
        "freeze . mapMaybe f = U.mapMaybe f"
        [ testWithGens "-> Int" $ checkMapMaybe $ F.int $ F.withOrigin (-10, 10) 0
        , testWithGens "-> Bool" $ checkMapMaybe $ F.bool True
        , testWithGens "-> Double" $ checkMapMaybe $ doubleG 8
        ]
    ]

test_filter :: TestTree
test_filter =
  testGroup
    "filter"
    [testWithGens "freeze . filter p = U.filter p" checkFilter]

test_mapSame :: TestTree
test_mapSame =
  testGroup
    "mapSame"
    [testWithGens "freeze . mapSame f = U.map f" checkMapSame]

checkMapMaybe ::
  ( F.Function a
  , U.Unbox a
  , Show a
  , Show b
  , Eq b
  , U.Unbox b
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
  let resl = U.mapMaybe f $ U.fromList xs
  label
    "% length of reduced vector"
    [classifyPercent (U.length resl) $ fromIntegral len]
  F.assert $
    P.expect resl
      .$ ( "actual"
         , unur PL.$
            linearly \l ->
              LUV.freeze PL.$
                LUV.mapMaybe (LUV.fromListL xs l) f
         )

checkFilter ::
  ( F.Function a
  , U.Unbox a
  , Show a
  , Eq a
  ) =>
  Gen a ->
  Property' String ()
checkFilter g = do
  (len, xs) <- genLenList g
  F.Fn p <- F.gen $ F.fun $ F.bool False
  let resl = U.filter p $ U.fromList xs
  label
    "% length of reduced vector"
    [classifyPercent (U.length resl) $ fromIntegral len]
  F.assert $
    P.expect resl
      .$ ( "actual"
         , unur PL.$
            linearly \l ->
              LUV.freeze PL.$
                LUV.filter (LUV.fromListL xs l) p
         )

checkMapSame ::
  ( F.Function a
  , U.Unbox a
  , Show a
  , Eq a
  ) =>
  Gen a ->
  Property' String ()
checkMapSame g = do
  (_len, xs) <- genLenList g
  F.Fn f <- F.gen $ F.fun g
  let resl = U.map f $ U.fromList xs
  F.assert $
    P.expect resl
      .$ ( "actual"
         , unur PL.$
            linearly \l ->
              LUV.freeze PL.$
                LUV.mapSame (LUV.fromListL xs l) f
         )

checkPushSnoc :: (Eq a, Show a, U.Unbox a) => Gen a -> Property' String ()
checkPushSnoc g = do
  (_l, xs) <- genLenList g
  x <- F.gen g
  F.assert $
    P.expect (U.fromList xs `U.snoc` x)
      .$ ( "actual"
         , unur PL.$
            linearly PL.$ \l ->
              LUV.freeze PL.$
                LUV.push x PL.$
                  LUV.fromListL xs l
         )

test_slice :: TestTree
test_slice =
  testGroup
    "slice"
    [ testWithGens "freeze . slice off len xs = slice off len " \g -> do
        (len, xs) <- genLenList g
        Slice {..} <- genSlice len
        let sliced = U.slice offset range $ U.fromList xs
        F.assert $
          P.expect sliced
            .$ ( "actual"
               , unur PL.$ linearly PL.$ \l ->
                  LUV.freeze PL.$
                    LUV.slice offset range (LUV.fromListL xs l)
               )
    ]

test_doubleAlloc :: TestTree
test_doubleAlloc =
  testGroup
    "can be allocated inside linearly twice"
    [ testDoubleAlloc (F.int $ F.between (-10, 10)) (F.int $ F.between (-10, 10)) LUV.fromListL LUV.fromListL (Ur.lift U.toList PL.. LUV.freeze) (Ur.lift U.toList PL.. LUV.freeze)
    , testDoubleAlloc (F.int $ F.between (-10, 10)) (F.bool True) LUV.fromListL LUV.fromListL (Ur.lift U.toList PL.. LUV.freeze) (Ur.lift U.toList PL.. LUV.freeze)
    , testDoubleAlloc (F.int $ F.between (-10, 10)) (doubleG 8) LUV.fromListL LUV.fromListL (Ur.lift U.toList PL.. LUV.freeze) (Ur.lift U.toList PL.. LUV.freeze)
    , testDoubleAlloc (F.bool True) (F.int $ F.between (-10, 10)) LUV.fromListL LUV.fromListL (Ur.lift U.toList PL.. LUV.freeze) (Ur.lift U.toList PL.. LUV.freeze)
    , testDoubleAlloc (F.bool True) (F.bool True) LUV.fromListL LUV.fromListL (Ur.lift U.toList PL.. LUV.freeze) (Ur.lift U.toList PL.. LUV.freeze)
    , testDoubleAlloc (F.bool True) (doubleG 8) LUV.fromListL LUV.fromListL (Ur.lift U.toList PL.. LUV.freeze) (Ur.lift U.toList PL.. LUV.freeze)
    , testDoubleAlloc (doubleG 8) (F.int $ F.between (-10, 10)) LUV.fromListL LUV.fromListL (Ur.lift U.toList PL.. LUV.freeze) (Ur.lift U.toList PL.. LUV.freeze)
    , testDoubleAlloc (doubleG 8) (F.bool True) LUV.fromListL LUV.fromListL (Ur.lift U.toList PL.. LUV.freeze) (Ur.lift U.toList PL.. LUV.freeze)
    , testDoubleAlloc (doubleG 8) (doubleG 8) LUV.fromListL LUV.fromListL (Ur.lift U.toList PL.. LUV.freeze) (Ur.lift U.toList PL.. LUV.freeze)
    ]
