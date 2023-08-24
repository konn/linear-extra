{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Mutable.Linear.UnboxedSpec (
  test_alloc,
  test_unsafeAlloc,
  test_unsafeAllocL,
  test_allocL,
  test_fromList,
  test_fromListL,
  test_set,
  test_fill,
  test_map,
  test_mapSame,
  test_findIndex,
  test_unsafeResize,
  test_unsafeSlice,
  test_serialAccess,
) where

import Data.Alloc.Linearly.Token (linearly)
import qualified Data.Array.Mutable.Linear.Unboxed as LUA
import Data.Functor ((<&>))
import Data.List (findIndex)
import Data.Maybe
import Data.Unrestricted.Linear (unur)
import qualified Data.Unrestricted.Linear as Ur
import qualified Data.Vector.Unboxed as U
import Linear.Array.Extra.TestUtils
import qualified Prelude.Linear as PL
import qualified Test.Falsify.Generator as F
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as F
import Test.Tasty
import Test.Tasty.Falsify
import qualified Test.Tasty.Falsify as F

test_alloc :: TestTree
test_alloc =
  testGroup
    "alloc"
    [ testWithGens
        "unur (alloc n x freeze) = replicate n x"
        \g -> do
          len <- gen $ F.int (F.between (0, 128))
          label "length" [classifyRangeBy 16 len]
          x <- gen g
          F.assert $
            P.expect (U.replicate len x)
              .$ ("alloc", unur (LUA.alloc len x LUA.freeze))
    ]

test_allocL :: TestTree
test_allocL =
  testGroup
    "allocL"
    [ testWithGens
        "linearly (\\l -> freeze (allocL l n x)) = alloc n x freeze"
        \g -> do
          len <- gen $ F.int (F.between (0, 128))
          x <- gen g
          label "length" [classifyRangeBy 16 len]
          F.assert $
            P.eq
              .$ ("alloc", unur (LUA.alloc len x LUA.freeze))
              .$ ("allocL", unur (linearly \l -> LUA.freeze (LUA.allocL l len x)))
    ]

test_unsafeAlloc :: TestTree
test_unsafeAlloc =
  testGroup
    "unsafeAlloc"
    [ testWithGens
        "unur (unsafeAllocL n (freeze  . fill x)) = replicate n x"
        \g -> do
          len <- gen $ F.int (F.between (0, 128))
          label "length" [classifyRangeBy 16 len]
          x <- gen g
          F.assert $
            P.expect (U.replicate len x)
              .$ ( "unsafeAlloc"
                 , unur PL.$
                    LUA.unsafeAlloc len PL.$
                      LUA.freeze PL.. LUA.fill x
                 )
    ]

test_unsafeAllocL :: TestTree
test_unsafeAllocL =
  testGroup
    "unsafeAllocL"
    [ testWithGens
        "unur (linearly \\l -> freeze (fill x (unsafeAllocL l n)) = replicate n x"
        \g -> do
          len <- gen $ F.int (F.between (0, 128))
          label "length" [classifyRangeBy 16 len]
          x <- gen g
          F.assert $
            P.expect (U.replicate len x)
              .$ ( "unsafeAlloc"
                 , unur PL.$ linearly \l ->
                    LUA.freeze PL.$ LUA.fill x PL.$ LUA.unsafeAllocL l len
                 )
    ]

test_fromList :: TestTree
test_fromList =
  testGroup
    "fromList"
    [ testWithGens
        "unur (fromList xs freeze) = U.fromList xs"
        \g -> do
          xs <- gen $ F.list (F.between (0, 128)) g
          label "length" [classifyRangeBy 16 $ length xs]
          F.assert $
            P.eq
              .$ ("alloc", unur (LUA.fromList xs LUA.freeze))
              .$ ("replicate", U.fromList xs)
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
              .$ ("alloc", unur (linearly \l -> LUA.freeze PL.$ LUA.fromListL l xs))
              .$ ("replicate", U.fromList xs)
    ]

test_set :: TestTree
test_set =
  testGroup
    "set"
    [ testWithGens
        "U.index i (unur (linearly \\l -> freeze (set i x (allocL l y)))) == x"
        \g -> do
          len <- gen $ F.int (F.between (1, 128))
          label "length" [classifyRangeBy 16 len]
          i <- gen $ F.int (F.between (0, len - 1))
          x <- gen g
          y <- gen g
          collect "x == y" [show $ x == y]
          F.assert $
            P.expect x
              .$ ("alloc", unur (linearly \l -> LUA.freeze PL.$ LUA.set i x (LUA.allocL l len y)) U.! i)
    , testWithGens
        "unur (linearly \\l -> fst' (get i (set i x (allocL l y)))) == x"
        \g -> do
          len <- gen $ F.int (F.between (1, 128))
          label "length" [classifyRangeBy 16 len]
          x <- gen g
          i <- gen $ F.int (F.between (0, len - 1))
          y <- gen g
          collect "x == y" [show $ x == y]
          F.assert $
            P.expect x
              .$ ( "alloc"
                 , unur
                    ( linearly \l ->
                        fst' PL.$ LUA.get i PL.$ LUA.set i x (LUA.allocL l len y)
                    )
                 )
    ]

test_fill :: TestTree
test_fill =
  testGroup
    "fill"
    [ testGroup
        "Sets all elements to the same"
        [ testWithGens "unsafeAllocL" \g -> do
            len <- gen $ F.int (F.between (1, 128))
            label "length" [classifyRangeBy 16 len]
            x <- gen g
            F.assert $
              P.expect (U.replicate len x)
                .$ ( "filled"
                   , unur PL.$ linearly \l ->
                      LUA.freeze (LUA.fill x (LUA.unsafeAllocL l len))
                   )
        , testWithGens "fromListL" \g -> do
            xs <- gen $ F.list (F.between (0, 128)) g
            let len = length xs
            label "length" [classifyRangeBy 16 len]
            x <- gen g
            F.assert $
              P.expect (U.replicate len x)
                .$ ( "filled"
                   , unur PL.$ linearly \l ->
                      LUA.freeze (LUA.fill x (LUA.fromListL l xs))
                   )
        ]
    ]

test_map :: TestTree
test_map =
  testGroup
    "map"
    [ testGroup
        "commutes with freeze"
        [ testProperty "Int -> Int" $
            testMapLikeFor LUA.map (F.int $ F.withOrigin (-10, 10) 0) (F.int $ F.between (-20, 20))
        , testProperty "Int -> Bool" $
            testMapLikeFor LUA.map (F.int $ F.withOrigin (-10, 10) 0) (F.bool True)
        , testProperty "Int -> Double" $
            testMapLikeFor LUA.map (F.int $ F.withOrigin (-10, 10) 0) (F.properFraction 32 <&> \(F.ProperFraction d) -> d)
        , testProperty "Bool -> Bool" $
            testMapLikeFor LUA.map (F.bool True) (F.bool True)
        , testProperty "Double -> Double" $
            testMapLikeFor LUA.map (doubleG 8) (doubleG 8)
        ]
    ]

test_mapSame :: TestTree
test_mapSame =
  testGroup
    "mapSame"
    [ testGroup
        "commutes with freeze"
        [ testProperty "Int -> Int" $
            testMapLikeFor LUA.mapSame (F.int $ F.withOrigin (-10, 10) 0) (F.int $ F.between (-20, 20))
        , testProperty "Bool -> Bool" $
            testMapLikeFor LUA.mapSame (F.bool True) (F.bool True)
        , testProperty "Double -> Double" $
            testMapLikeFor LUA.mapSame (doubleG 8) (doubleG 8)
        ]
    ]

testMapLikeFor ::
  (Eq b, U.Unbox a, Show a, U.Unbox b, Show b, F.Function a) =>
  ((a -> b) -> LUA.UArray a %1 -> LUA.UArray b) ->
  Gen a ->
  Gen b ->
  F.Property ()
testMapLikeFor mapLike arg ret = do
  xs <- F.gen $ F.list (F.between (0, 128)) arg
  Fn f <- F.gen $ F.fun ret
  let len = length xs
  label "length" [classifyRangeBy 16 len]
  F.assert $
    P.expect (U.map f $ U.fromList xs)
      .$ ( "mapped"
         , unur PL.$ linearly \l ->
            LUA.freeze (mapLike f (LUA.fromListL l xs))
         )

test_findIndex :: TestTree
test_findIndex =
  testGroup
    "findIndex"
    [testWithGens "commutes with fromListL" testFindIndex]

testFindIndex :: (U.Unbox a, Show a, F.Function a) => Gen a -> Property ()
testFindIndex argG = do
  Fn p <- F.gen $ F.fun $ F.bool True
  xs <- F.gen $ F.list (F.between (0, 128)) argG
  let resl = findIndex p xs
  let len = length xs
  label "length" [classifyRangeBy 16 len]
  collect "found" [isJust resl]
  F.assert $
    P.expect resl
      .$ ( "linear"
         , unur PL.$ linearly \l ->
            fst' (LUA.findIndex p (LUA.fromListL l xs))
         )

test_unsafeResize :: TestTree
test_unsafeResize =
  testGroup
    "unsafeResize"
    [ testWithGens
        "is no-op on when fed with the same length"
        checkUnsafeResizeSame
    , testWithGens
        "(freeze . unsafeResize n = U.take n) for n < len"
        checkUnsafeResizeShorter
    , testWithGens
        "usafeResize (len + n) has size (len + n) and coincides with the original for initial n-elements"
        checkUnsafeResizeLarger
    ]

checkUnsafeResizeSame :: (Show a, Eq a, U.Unbox a) => Gen a -> Property ()
checkUnsafeResizeSame g = do
  xs <- gen $ F.list (F.between (0, 128)) g
  let len = length xs
  label "length" [classifyRangeBy 16 len]
  F.assert $
    P.satisfies ("pair", uncurry (==))
      .$ ( "paired"
         , unur PL.$ linearly \l ->
            PL.dup2 (LUA.fromListL l xs) PL.& \(ls, rs) ->
              Ur.lift2
                (,)
                (LUA.freeze ls)
                (LUA.freeze (LUA.unsafeResize len rs))
         )

checkUnsafeResizeShorter :: (Show a, Eq a, U.Unbox a) => Gen a -> Property ()
checkUnsafeResizeShorter g = do
  xs <- gen $ F.list (F.between (0, 128)) g
  let len = length xs
  label "length" [classifyRangeBy 16 len]
  smal <- genWith (\a -> Just $ "Shrink size: " <> show a) $ F.int $ F.between (0, len)
  F.assert $
    P.expect (U.take smal $ U.fromList xs)
      .$ ( "generated"
         , unur PL.$ linearly \l ->
            LUA.freeze PL.$ LUA.unsafeResize smal PL.$ LUA.fromListL l xs
         )

checkUnsafeResizeLarger :: (Show a, Eq a, U.Unbox a) => Gen a -> Property ()
checkUnsafeResizeLarger g = do
  xs <- gen $ F.list (F.between (0, 128)) g
  let len = length xs
  label "length" [classifyRangeBy 16 len]
  growth <- genWith (\a -> Just $ "Growth size: " <> show a) $ F.int $ F.between (1, 128)
  F.assert $
    P.expect (len + growth)
      .$ ( "actual size"
         , U.length $ unur PL.$ linearly \l ->
            LUA.freeze PL.$ LUA.unsafeResize (len + growth) PL.$ LUA.fromListL l xs
         )
  F.assert $
    P.expect (U.fromList xs)
      .$ ( "actual initial segment"
         , U.take len $ unur PL.$ linearly \l ->
            LUA.freeze PL.$ LUA.unsafeResize (len + growth) PL.$ LUA.fromListL l xs
         )

test_unsafeSlice :: TestTree
test_unsafeSlice =
  testGroup
    "unsafeSlice"
    [testWithGens "commutes with freeze . snd'" checkUnsafeSlice]

checkUnsafeSlice :: (Show a, Eq a, U.Unbox a) => Gen a -> Property ()
checkUnsafeSlice g = do
  xs <- gen $ F.list (F.between (0, 128)) g
  let len = length xs
  label "length" [classifyRangeBy 16 len]
  off <- gen $ F.int $ F.between (0, len)
  label
    "% offset"
    [ if len == 0
        then "N/A"
        else classifyRangeBy 10 $ 100 * off `quot` len
    ]
  ran <- gen $ F.int $ F.between (0, len - off)
  label "range" [classifyRangeBy 16 ran]
  F.assert $
    P.expect (U.unsafeSlice off ran $ U.fromList xs)
      .$ ( "actual slice"
         , unur PL.$ linearly \l ->
            LUA.freeze PL.$ snd' PL.$ LUA.unsafeSlice off ran PL.$ LUA.fromListL l xs
         )

test_serialAccess :: TestTree
test_serialAccess =
  testGroup
    "Serial Updates has the same meaning with vectors"
    [ testProperty "Int" $
        checkSerialUpdateSemantics
          (F.int $ F.between (-10, 10))
          LUA.fromListL
          LUA.freeze
    , testProperty "Bool" $
        checkSerialUpdateSemantics
          (F.bool True)
          LUA.fromListL
          LUA.freeze
    , testProperty "Double" $
        checkSerialUpdateSemantics
          (doubleG 8)
          LUA.fromListL
          LUA.freeze
    ]
