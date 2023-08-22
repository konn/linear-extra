{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}

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
) where

import Data.Alloc.Linearly.Token (linearly)
import qualified Data.Array.Mutable.Linear.Unboxed as LUA
import Data.Functor ((<&>))
import Data.List (findIndex)
import Data.Maybe
import Data.Unrestricted.Linear (unur)
import qualified Data.Unrestricted.Linear as Ur
import qualified Data.Vector.Unboxed as U
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
    [ testGroup
        "unur (alloc n x freeze) = replicate n x"
        [ testProperty "Int" do
            len <- gen $ F.int (F.between (0, 128))
            label "length" [classifyRangeBy 16 len]
            x <- gen $ F.int (F.withOrigin (-10, 10) 0)
            F.assert $
              P.expect (U.replicate len x)
                .$ ("alloc", unur (LUA.alloc len x LUA.freeze))
        , testProperty "Bool" do
            len <- gen $ F.int (F.between (0, 128))
            label "length" [classifyRangeBy 16 len]
            x <- gen $ F.bool True
            F.assert $
              P.expect (U.replicate len x)
                .$ ("alloc", unur (LUA.alloc len x LUA.freeze))
        ]
    ]

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
                .$ ("alloc", unur (LUA.alloc len x LUA.freeze))
                .$ ("allocL", unur (linearly \l -> LUA.freeze (LUA.allocL l len x)))
        ]
    ]

test_unsafeAlloc :: TestTree
test_unsafeAlloc =
  testGroup
    "unsafeAlloc"
    [ testGroup
        "unur (unsafeAllocL n (freeze  . fill x)) = replicate n x"
        [ testProperty "Int" do
            len <- gen $ F.int (F.between (0, 128))
            label "length" [classifyRangeBy 16 len]
            x <- gen $ F.int (F.withOrigin (-10, 10) 0)
            F.assert $
              P.expect (U.replicate len x)
                .$ ( "unsafeAlloc"
                   , unur PL.$
                      LUA.unsafeAlloc len PL.$
                        LUA.freeze PL.. LUA.fill x
                   )
        ]
    ]

test_unsafeAllocL :: TestTree
test_unsafeAllocL =
  testGroup
    "unsafeAllocL"
    [ testGroup
        "unur (linearly \\l -> freeze (fill x (unsafeAllocL l n)) = replicate n x"
        [ testProperty "Int" do
            len <- gen $ F.int (F.between (0, 128))
            label "length" [classifyRangeBy 16 len]
            x <- gen $ F.int (F.withOrigin (-10, 10) 0)
            F.assert $
              P.expect (U.replicate len x)
                .$ ( "unsafeAlloc"
                   , unur PL.$ linearly \l ->
                      LUA.freeze PL.$ LUA.fill x PL.$ LUA.unsafeAllocL l len
                   )
        ]
    ]

classifyRangeBy :: Int -> Int -> String
classifyRangeBy _ 0 = "0"
classifyRangeBy q n =
  let nDiv = n `quot` q
   in show (nDiv * q + 1, (nDiv + 1) * q)

test_fromList :: TestTree
test_fromList =
  testGroup
    "fromList"
    [ testGroup
        "unur (fromList xs freeze) = U.fromList xs"
        [ testProperty "Int" do
            xs <- gen $ F.list (F.between (0, 128)) (F.int (F.withOrigin (-10, 10) 0))
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.eq
                .$ ("alloc", unur (LUA.fromList xs LUA.freeze))
                .$ ("replicate", U.fromList xs)
        , testProperty "Bool" do
            xs <- gen $ F.list (F.between (0, 128)) (F.bool True)
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.expect (U.fromList xs)
                .$ ("alloc", unur (LUA.fromList xs LUA.freeze))
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
                .$ ("alloc", unur (linearly \l -> LUA.freeze PL.$ LUA.fromListL l xs))
                .$ ("replicate", U.fromList xs)
        , testProperty "Bool" do
            xs <- gen $ F.list (F.between (0, 128)) (F.bool True)
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.expect (U.fromList xs)
                .$ ("alloc", unur (linearly \l -> LUA.freeze PL.$ LUA.fromListL l xs))
        ]
    ]

test_set :: TestTree
test_set =
  testGroup
    "set"
    [ testGroup
        "U.index i (unur (linearly \\l -> freeze (set i x (allocL l y)))) == x"
        [ testProperty "Int" do
            len <- gen $ F.int (F.between (1, 128))
            label "length" [classifyRangeBy 16 len]
            i <- gen $ F.int (F.between (0, len - 1))
            x <- gen $ F.int (F.withOrigin (-10, 10) 0)
            y <- gen $ F.int (F.withOrigin (-10, 10) 0)
            collect "x == y" [show $ x == y]
            F.assert $
              P.expect x
                .$ ("alloc", unur (linearly \l -> LUA.freeze PL.$ LUA.set i x (LUA.allocL l len y)) U.! i)
        ]
    , testGroup
        "unur (linearly \\l -> fst' (get i (set i x (allocL l y)))) == x"
        [ testProperty "Int" do
            len <- gen $ F.int (F.between (1, 128))
            label "length" [classifyRangeBy 16 len]
            i <- gen $ F.int (F.between (0, len - 1))
            x <- gen $ F.int (F.withOrigin (-10, 10) 0)
            y <- gen $ F.int (F.withOrigin (-10, 10) 0)
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
    ]

fst' :: PL.Consumable b => (a, b) %1 -> a
fst' = PL.uncurry (PL.flip PL.lseq)

test_fill :: TestTree
test_fill =
  testGroup
    "fill"
    [ testGroup
        "Sets all elements to the same"
        [ testProperty "unsafeAllocL" do
            len <- gen $ F.int (F.between (1, 128))
            label "length" [classifyRangeBy 16 len]
            x <- gen $ F.int (F.withOrigin (-10, 10) 0)
            F.assert $
              P.expect (U.replicate len x)
                .$ ( "filled"
                   , unur PL.$ linearly \l ->
                      LUA.freeze (LUA.fill x (LUA.unsafeAllocL l len))
                   )
        , testProperty "fromListL" do
            xs <- gen $ F.list (F.between (0, 128)) (F.int (F.withOrigin (-10, 10) 0))
            let len = length xs
            label "length" [classifyRangeBy 16 len]
            x <- gen $ F.int (F.withOrigin (-10, 10) 0)
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

doubleG :: F.Precision -> Gen Double
doubleG i = F.properFraction i <&> \(F.ProperFraction d) -> d

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
    [ testGroup
        "commutes with fromListL"
        [ testProperty "Int" $
            testFindIndex (F.int $ F.withOrigin (-10, 10) 0)
        , testProperty "Bool" $
            testFindIndex (F.bool True)
        , testProperty "Double" $
            testFindIndex (doubleG 8)
        ]
    ]

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
    [ testGroup
        "is no-op on when fed with the same length"
        [ testProperty "Int" $ checkUnsafeResizeSame $ F.int $ F.withOrigin (-10, 10) 0
        , testProperty "Double" $ checkUnsafeResizeSame $ doubleG 8
        , testProperty "Double" $ checkUnsafeResizeSame $ F.bool True
        ]
    , testGroup
        "(freeze . unsafeResize n = U.take n) for n < len"
        [ testProperty "Int" $ checkUnsafeResizeShorter $ F.int $ F.withOrigin (-10, 10) 0
        , testProperty "Double" $ checkUnsafeResizeShorter $ doubleG 8
        , testProperty "Double" $ checkUnsafeResizeShorter $ F.bool True
        ]
    , testGroup
        "usafeResize (len + n) has size (len + n) and coincides with the original for initial n-elements"
        [ testProperty "Int" $ checkUnsafeResizeLarger $ F.int $ F.withOrigin (-10, 10) 0
        , testProperty "Double" $ checkUnsafeResizeLarger $ doubleG 8
        , testProperty "Double" $ checkUnsafeResizeLarger $ F.bool True
        ]
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
