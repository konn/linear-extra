{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Array.Polarized.Push.ExtraSpec (test_alloc, consSnocsToList, consSnocsArray) where

import qualified Data.Array.Polarized as PA
import qualified Data.Array.Polarized.Pull.Extra as Pull
import qualified Data.Array.Polarized.Push.Extra as Push
import qualified Data.DList as DL
import qualified Data.List as List
import qualified Data.List.Linear as LinList
import qualified Data.Monoid.Linear as PL
import Data.Unrestricted.Linear (Ur (..))
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)
import Linear.Array.Extra.TestUtils
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
        "Push.alloc . transfer . Pull.fromVector = id"
        [ testWithGens "Boxed Vector" $ checkAllocFromVector @V.Vector
        , testWithGens "Unboxed Vector" $ checkAllocFromVector @U.Vector
        ]
    , testGroup
        "Push.alloc (make a n) = replicate n a"
        [ testWithGens "Boxed Vector" $ checkAllocMake @V.Vector
        , testWithGens "Unboxed Vector" $ checkAllocMake @U.Vector
        ]
    , testGroup
        "Push.alloc ((cons|snoc)* mempty) = id"
        [ testWithGens "Boxed Vector" $ checkConsSnocEmpty @V.Vector
        , testWithGens "Unboxed Vector" $ checkConsSnocEmpty @U.Vector
        ]
    ]

checkAllocFromVector ::
  forall v a.
  (G.Vector v a, Show (v a), Eq (v a), Show a) =>
  Gen a ->
  Property ()
checkAllocFromVector g = do
  (_l, xs) <- genLenList g
  let v = G.fromList @v @a xs
  F.assert $
    P.expect v
      .$ ( "actual"
         , Push.alloc (PA.transfer (Pull.fromVector v))
         )

checkAllocMake ::
  forall v a.
  (G.Vector v a, Show (v a), Eq (v a), Show a) =>
  Gen a ->
  Property ()
checkAllocMake g = do
  n <- F.gen $ F.int $ F.between (0, 128)
  label "length" [classifyRangeBy 16 n]
  x <- F.gen g
  let v = G.replicate @v @a n x
  F.assert $
    P.expect v
      .$ ( "actual"
         , Push.alloc (Push.make x n)
         )

data ConsSnoc a = Cons a | Snoc a
  deriving (Show, Eq, Ord, Generic)

consSnocsToList :: [ConsSnoc a] -> [a]
consSnocsToList =
  DL.toList
    . List.foldl'
      ( \xs -> \case
          Cons x -> DL.cons x xs
          Snoc x -> DL.snoc xs x
      )
      DL.empty

consSnocsArray :: [ConsSnoc a] -> Push.Array a %1 -> Push.Array a
consSnocsArray ops arr0 =
  LinList.foldl'
    ( \arr -> \case
        (Ur (Cons x)) -> Push.cons x arr
        (Ur (Snoc x)) -> Push.snoc x arr
    )
    arr0
    (map Ur ops)

checkConsSnocEmpty ::
  forall v a.
  (G.Vector v a, Show (v a), Eq (v a), Show a) =>
  Gen a ->
  Property ()
checkConsSnocEmpty g = do
  ops <-
    F.gen $
      F.list (F.between (0, 128)) $
        F.choose (Cons <$> g) (Snoc <$> g)
  label "length" [classifyRangeBy 16 $ length ops]
  let v = G.fromList @v $ consSnocsToList ops
  F.assert $
    P.expect v
      .$ ( "actual"
         , Push.alloc (consSnocsArray ops PL.mempty)
         )
