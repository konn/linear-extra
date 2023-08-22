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
) where

import Data.Alloc.Linearly.Token (linearly)
import qualified Data.Array.Mutable.Linear.Unboxed as LUA
import Data.Unrestricted.Linear (unur)
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
            x <- gen $ F.int (F.between (-10, 10))
            F.assert $
              P.eq
                .$ ("alloc", unur (LUA.alloc len x LUA.freeze))
                .$ ("replicate", U.replicate len x)
        , testProperty "Bool" do
            len <- gen $ F.int (F.between (0, 128))
            label "length" [classifyRangeBy 16 len]
            x <- gen $ F.bool True
            F.assert $
              P.eq
                .$ ("alloc", unur (LUA.alloc len x LUA.freeze))
                .$ ("replicate", U.replicate len x)
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
            x <- gen $ F.int (F.between (-10, 10))
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
            x <- gen $ F.int (F.between (-10, 10))
            F.assert $
              P.eq
                .$ ( "unsafeAlloc"
                   , unur PL.$
                      LUA.unsafeAlloc len PL.$
                        LUA.freeze PL.. LUA.fill x
                   )
                .$ ("replicate", U.replicate len x)
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
            x <- gen $ F.int (F.between (-10, 10))
            F.assert $
              P.eq
                .$ ( "unsafeAlloc"
                   , unur PL.$ linearly \l ->
                      LUA.freeze PL.$ LUA.fill x PL.$ LUA.unsafeAllocL l len
                   )
                .$ ("replicate", U.replicate len x)
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
            xs <- gen $ F.list (F.between (0, 128)) (F.int (F.between (-10, 10)))
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.eq
                .$ ("alloc", unur (LUA.fromList xs LUA.freeze))
                .$ ("replicate", U.fromList xs)
        , testProperty "Bool" do
            xs <- gen $ F.list (F.between (0, 128)) (F.bool True)
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.eq
                .$ ("alloc", unur (LUA.fromList xs LUA.freeze))
                .$ ("replicate", U.fromList xs)
        ]
    ]

test_fromListL :: TestTree
test_fromListL =
  testGroup
    "fromListL"
    [ testGroup
        "unur (linearly \\l -> freeze (fromListL l xs)) = U.fromList xs"
        [ testProperty "Int" do
            xs <- gen $ F.list (F.between (0, 128)) (F.int (F.between (-10, 10)))
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.eq
                .$ ("alloc", unur (linearly \l -> LUA.freeze PL.$ LUA.fromListL l xs))
                .$ ("replicate", U.fromList xs)
        , testProperty "Bool" do
            xs <- gen $ F.list (F.between (0, 128)) (F.bool True)
            label "length" [classifyRangeBy 16 $ length xs]
            F.assert $
              P.eq
                .$ ("alloc", unur (linearly \l -> LUA.freeze PL.$ LUA.fromListL l xs))
                .$ ("replicate", U.fromList xs)
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
            x <- gen $ F.int (F.between (-10, 10))
            y <- gen $ F.int (F.between (-10, 10))
            collect "x == y" [show $ x == y]
            F.assert $
              P.eq
                .$ ("alloc", unur (linearly \l -> LUA.freeze PL.$ LUA.set i x (LUA.allocL l len y)) U.! i)
                .$ ("replicate", x)
        ]
    , testGroup
        "unur (linearly \\l -> fst' (get i (set i x (allocL l y)))) == x"
        [ testProperty "Int" do
            len <- gen $ F.int (F.between (1, 128))
            label "length" [classifyRangeBy 16 len]
            i <- gen $ F.int (F.between (0, len - 1))
            x <- gen $ F.int (F.between (-10, 10))
            y <- gen $ F.int (F.between (-10, 10))
            collect "x == y" [show $ x == y]
            F.assert $
              P.eq
                .$ ( "alloc"
                   , unur
                      ( linearly \l ->
                          fst' PL.$ LUA.get i PL.$ LUA.set i x (LUA.allocL l len y)
                      )
                   )
                .$ ("replicate", x)
        ]
    ]

fst' :: PL.Consumable b => (a, b) %1 -> a
fst' = PL.uncurry (PL.flip PL.lseq)
