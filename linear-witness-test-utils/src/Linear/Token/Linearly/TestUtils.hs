{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Linear.Token.Linearly.TestUtils (
  classifyRangeBy,
  fst',
  snd',
  doubleG,
  arrayOpG,
  ArrayOp (..),
  HasArrayOp (..),
  applyArrayOps,
  applyArrayOpsL,
  checkSerialUpdateSemantics,
  testDoubleAlloc,
  testDoubleAllocSnoc,
  checkDoubleAlloc,
  classifyPercent,
  testWithGens,
  genLenList,
  distribUr,
  Slice (..),
  genSlice,
) where

import qualified Control.Foldl as Foldl
import qualified Data.Array.Mutable.Linear as LA
import qualified Data.Foldable as F
import Data.Functor ((<&>))
import Data.List (foldl')
import qualified Data.List.Linear as L
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Mutable.Linear as LV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Generics (Generic)
import Linear.Token.Linearly (Linearly, besides, linearly)
import Prelude.Linear (Ur (..), unur)
import qualified Prelude.Linear as PL
import qualified Test.Falsify.Generator as F
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as F
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Tasty.Falsify as F
import Type.Reflection (Typeable, typeRep)
import qualified Unsafe.Linear as Unsafe

classifyRangeBy :: (Show i, Integral i) => i -> i -> String
classifyRangeBy _ 0 = "0"
classifyRangeBy q n =
  let nDiv = (n - 1) `quot` q
   in show (nDiv * q + 1, (nDiv + 1) * q)

fst' :: (PL.Consumable b) => (a, b) %1 -> a
fst' = PL.uncurry (PL.flip PL.lseq)

snd' :: (PL.Consumable a) => (a, b) %1 -> b
snd' = PL.uncurry PL.lseq

doubleG :: F.Precision -> F.Gen Double
doubleG i = F.properFraction i <&> \(F.ProperFraction d) -> d

data ArrayOp a
  = Get !Int
  | Set !Int !a
  | Modify !Int (F.Fun a a)
  | Map (F.Fun a a)
  deriving (Show, Generic)

arrayOpG :: (F.Function a) => Word -> F.Gen a -> F.Gen (ArrayOp a)
arrayOpG len g =
  F.frequency
    [ (1, Get <$> F.int (F.between (0, fromIntegral len - 1)))
    , (1, Set <$> F.int (F.between (0, fromIntegral len - 1)) <*> g)
    , (1, Modify <$> F.int (F.between (0, fromIntegral len - 1)) <*> F.fun g)
    , (1, Map <$> F.fun g)
    ]

class HasArrayOp a t | t -> a where
  applyArrayOp :: ArrayOp a -> t %1 -> t

instance HasArrayOp a [a] where
  applyArrayOp (Get _) = PL.id
  applyArrayOp (Set i x) = \xs ->
    L.splitAt i xs PL.& \(lh, rh) -> lh L.++ (x : rh)
  applyArrayOp (Map (F.Fn f)) = L.map $ Unsafe.toLinear f
  applyArrayOp (Modify i (F.Fn f)) = \xs ->
    L.splitAt i xs PL.& \case
      (lh, []) -> error ("applyArrayOp.List.Modify: empty (" <> show i <> ")") lh
      (lh, x : xs') -> lh PL.++ Unsafe.toLinear f x : xs'

instance HasArrayOp a (LA.Array a) where
  applyArrayOp (Get i) = \xs -> LA.get i xs PL.& \(Ur _, xs') -> xs'
  applyArrayOp (Set i x) = LA.set i x
  applyArrayOp (Modify i (F.Fn f)) = \xs ->
    LA.get i xs PL.& \(Ur x, xs') -> LA.set i (f x) xs'
  applyArrayOp (Map (F.Fn f)) = LA.map f

instance HasArrayOp a (LV.Vector a) where
  applyArrayOp (Get i) = \xs -> LV.get i xs PL.& \(Ur _, xs') -> xs'
  applyArrayOp (Set i x) = LV.set i x
  applyArrayOp (Modify i (F.Fn f)) = LV.modify_ f i
  applyArrayOp (Map (F.Fn f)) = PL.flip LV.mapMaybe (Just . f)

instance HasArrayOp a (V.Vector a) where
  applyArrayOp (Get _) = PL.id
  applyArrayOp (Set i x) =
    Unsafe.toLinear PL.$
      V.modify (\mv -> MV.write mv i x)
  applyArrayOp (Modify i (F.Fn f)) =
    Unsafe.toLinear PL.$
      V.modify (\mv -> MV.modify mv f i)
  applyArrayOp (Map (F.Fn f)) = Unsafe.toLinear (V.map f)

instance (U.Unbox a) => HasArrayOp a (U.Vector a) where
  applyArrayOp (Get _) = PL.id
  applyArrayOp (Set i x) =
    Unsafe.toLinear PL.$
      U.modify (\mv -> MU.write mv i x)
  applyArrayOp (Modify i (F.Fn f)) =
    Unsafe.toLinear PL.$
      U.modify (\mv -> MU.modify mv f i)
  applyArrayOp (Map (F.Fn f)) = Unsafe.toLinear (U.map f)

applyArrayOps :: (HasArrayOp a t) => [ArrayOp a] -> t -> t
{-# ANN applyArrayOps "HLint: ignore Avoid lambda" #-}
applyArrayOps = flip $ foldl' (\xs op -> applyArrayOp op xs)

applyArrayOpsL :: (HasArrayOp a t) => [ArrayOp a] -> t %1 -> t
{-# ANN applyArrayOpsL "HLint: ignore Avoid lambda" #-}
applyArrayOpsL ops a = L.foldl' (\x (Ur o) -> applyArrayOp o x) a (map Ur ops)

testDoubleAlloc ::
  ( Show a
  , Show b
  , Eq a
  , Eq b
  , Typeable a
  , Typeable b
  ) =>
  F.Gen a ->
  F.Gen b ->
  ([a] -> Linearly %1 -> t a) ->
  ([b] -> Linearly %1 -> t b) ->
  (t a %1 -> Ur [a]) ->
  (t b %1 -> Ur [b]) ->
  TestTree
testDoubleAlloc (ga :: F.Gen a) (gb :: F.Gen b) fromLstA fromLstB frzA frzB =
  testProperty (show (typeRep @a, typeRep @b)) $
    checkDoubleAlloc ga gb fromLstA fromLstB frzA frzB

checkDoubleAlloc ::
  ( Show a
  , Show b
  , Eq a
  , Eq b
  ) =>
  F.Gen a ->
  F.Gen b ->
  ([a] -> Linearly %1 -> t a) ->
  ([b] -> Linearly %1 -> t b) ->
  (t a %1 -> Ur [a]) ->
  (t b %1 -> Ur [b]) ->
  F.Property ()
checkDoubleAlloc ga gb fromLstA fromLstB frzA frzB = do
  len1 <- F.gen $ F.integral $ F.between (1, 10)
  len2 <- F.gen $ F.integral $ F.between (1, 10)
  F.label "length1" [classifyRangeBy 16 len1]
  F.label "length2" [classifyRangeBy 16 len2]
  xs <- F.gen $ F.list (F.between (len1, len1)) ga
  ys <- F.gen $ F.list (F.between (len2, len2)) gb
  F.assert $
    P.expect (xs, ys)
      .$ ( "actual"
         , unur
            ( linearly \l ->
                besides l (fromLstA xs) PL.& \(xs', l') ->
                  fromLstB ys l' PL.& \ys' ->
                    distribUr (frzA xs', frzB ys')
            )
         )

testDoubleAllocSnoc ::
  ( Show a
  , Show b
  , Eq a
  , Eq b
  , Typeable a
  , Typeable b
  ) =>
  F.Gen a ->
  F.Gen b ->
  (Linearly %1 -> t a) ->
  (Linearly %1 -> t b) ->
  (a -> t a %1 -> t a) ->
  (b -> t b %1 -> t b) ->
  (t a %1 -> Ur [a]) ->
  (t b %1 -> Ur [b]) ->
  TestTree
testDoubleAllocSnoc (ga :: F.Gen a) (gb :: F.Gen b) empA empB snocA snocB frzA frzB =
  testProperty (show (typeRep @a, typeRep @b)) $
    checkDoubleAllocSnoc ga gb empA empB snocA snocB frzA frzB

checkDoubleAllocSnoc ::
  ( Show a
  , Show b
  , Eq a
  , Eq b
  ) =>
  F.Gen a ->
  F.Gen b ->
  (Linearly %1 -> t a) ->
  (Linearly %1 -> t b) ->
  (a -> t a %1 -> t a) ->
  (b -> t b %1 -> t b) ->
  (t a %1 -> Ur [a]) ->
  (t b %1 -> Ur [b]) ->
  F.Property ()
checkDoubleAllocSnoc ga gb empA empB snocA snocB frzA frzB = do
  len1 <- F.gen $ F.integral $ F.between (1, 10)
  len2 <- F.gen $ F.integral $ F.between (1, 10)
  F.label "length1" [classifyRangeBy 16 len1]
  F.label "length2" [classifyRangeBy 16 len2]
  xs <- F.gen $ F.list (F.between (len1, len1)) ga
  ys <- F.gen $ F.list (F.between (len2, len2)) gb
  F.assert $
    P.expect (xs, ys)
      .$ ( "actual"
         , unur
            ( linearly \l ->
                besides l empA PL.& \(xs', l') ->
                  empB l' PL.& \ys' ->
                    distribUr
                      ( frzA (L.foldl' (\ls (Ur x) -> snocA x ls) xs' (map Ur xs))
                      , frzB (L.foldl' (\ls (Ur y) -> snocB y ls) ys' (map Ur ys))
                      )
            )
         )

checkSerialUpdateSemantics ::
  (Show a, F.Function a, Eq (v a), HasArrayOp a (v a), G.Vector v a, HasArrayOp a x, Show (v a)) =>
  F.Gen a ->
  ([a] -> Linearly %1 -> x) ->
  (x %1 -> Ur (v a)) ->
  F.Property ()
checkSerialUpdateSemantics g fromLst frz = do
  len <- F.gen $ F.integral $ F.between (1, 10)
  F.label "length" [classifyRangeBy 16 len]
  xs <- F.gen $ F.list (F.between (len, len)) g
  upds <- F.gen $ F.list (F.between (0, 50)) $ arrayOpG len g
  let (opCount, numSet, numGet, numMap, numMod, numInsts, collide) =
        Foldl.fold
          ( (,,,,,,)
              <$> Foldl.length
              <*> Foldl.prefilter (\case Set {} -> True; _ -> False) Foldl.length
              <*> Foldl.prefilter (\case Get {} -> True; _ -> False) Foldl.length
              <*> Foldl.prefilter (\case Map {} -> True; _ -> False) Foldl.length
              <*> Foldl.prefilter (\case Modify {} -> True; _ -> False) Foldl.length
              <*> Foldl.premap classifyOp (length <$> Foldl.set)
              <*> ( length . filter (> 1) . F.toList
                      <$> Foldl.premap
                        (fmap (,()) <$> opIndex)
                        (Foldl.handles Foldl.folded $ Foldl.foldByKeyMap Foldl.length)
                  )
          )
          upds
  F.label "# of updates" [classifyRangeBy 5 opCount]
  F.label "# of index collision" [classifyRangeBy 1 collide]
  F.label "# of distinct ops" [show numInsts]
  F.label "% sets" [classifyPercent numSet opCount]
  F.label "% get" [classifyPercent numGet opCount]
  F.label "% mods" [classifyPercent numMod opCount]
  F.label "% maps" [classifyPercent numMap opCount]
  F.assert $
    P.expect (applyArrayOps upds $ G.fromList xs)
      .$ ( "array"
         , unur PL.$
            linearly PL.$
              frz PL.. applyArrayOpsL upds PL.. fromLst xs
         )

opIndex :: ArrayOp a -> Maybe Int
opIndex = \case
  Set i _ -> pure i
  Get i -> pure i
  Map {} -> Nothing
  Modify i _ -> pure i

classifyOp :: ArrayOp a -> Int
classifyOp = \case
  Set {} -> 0 :: Int
  Get {} -> 1
  Map {} -> 2
  Modify {} -> 3

classifyPercent :: (Show i, Integral i) => i -> i -> String
classifyPercent _ 0 = "-"
classifyPercent i j = classifyRangeBy 10 $ 100 * i `quot` j

testWithGens ::
  String ->
  (forall a. (Show a, Eq a, F.Function a, U.Unbox a) => F.Gen a -> F.Property ()) ->
  TestTree
testWithGens l p =
  testGroup
    l
    [ F.testProperty "Int" $ p $ F.int $ F.withOrigin (-10, 10) 0
    , F.testProperty "Bool" $ p $ F.bool True
    , F.testProperty "Double" $ p $ doubleG 8
    ]

genLenList :: (Show a) => F.Gen a -> F.Property' e (Word, [a])
genLenList g = do
  len <- F.gen $ F.integral (F.between (0, 128))
  F.label "length" [classifyRangeBy 16 len]
  xs <- F.gen $ F.list (F.between (len, len)) g
  pure (len, xs)

distribUr :: (Ur a, Ur b) %1 -> Ur (a, b)
distribUr (Ur l, Ur r) = Ur (l, r)

data Slice = Slice {offset, range :: {-# UNPACK #-} !Int}
  deriving (Show, Eq, Ord, Generic)

genSlice :: Word -> F.Property Slice
genSlice (fromIntegral -> len) = do
  offset <- F.gen $ F.int $ F.between (0, len)
  F.label "% offset" [classifyPercent offset len]
  range <- F.gen $ F.int $ F.between (0, len - offset)
  F.label "% range" [classifyPercent range len]
  pure Slice {..}

#if !MIN_VERSION_linear_base(0,4,0)
instance (Semigroup a) => PL.Semigroup (Ur a) where
  Ur a <> Ur b = Ur (a <> b)

instance (Monoid a) => PL.Monoid (Ur a) where
  mempty = Ur mempty
#endif
