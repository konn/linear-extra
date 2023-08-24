{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}

module Linear.Array.Extra.TestUtils (
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
  classifyPercent,
) where

import qualified Control.Foldl as Foldl
import Data.Alloc.Linearly.Token (Linearly, linearly)
import qualified Data.Array.Mutable.Linear.Extra as LA
import qualified Data.Array.Mutable.Linear.Unboxed as LUA
import Data.Functor ((<&>))
import Data.List (foldl')
import qualified Data.List.Linear as L
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Mutable.Linear.Extra as LV
import qualified Data.Vector.Mutable.Linear.Unboxed as LUV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Generics (Generic)
import Prelude.Linear (Ur (..), unur)
import qualified Prelude.Linear as PL
import qualified Test.Falsify.Generator as F
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as F
import qualified Test.Tasty.Falsify as F
import qualified Unsafe.Linear as Unsafe

classifyRangeBy :: Int -> Int -> String
classifyRangeBy _ 0 = "0"
classifyRangeBy q n =
  let nDiv = (n - 1) `quot` q
   in show (nDiv * q + 1, (nDiv + 1) * q)

fst' :: PL.Consumable b => (a, b) %1 -> a
fst' = PL.uncurry (PL.flip PL.lseq)

snd' :: PL.Consumable a => (a, b) %1 -> b
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

instance U.Unbox a => HasArrayOp a (LUA.UArray a) where
  applyArrayOp (Get i) = \xs -> LUA.get i xs PL.& \(Ur _, xs') -> xs'
  applyArrayOp (Set i x) = LUA.set i x
  applyArrayOp (Modify i (F.Fn f)) = \xs ->
    LUA.get i xs PL.& \(Ur x, xs') -> LUA.set i (f x) xs'
  applyArrayOp (Map (F.Fn f)) = LUA.map f

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

instance U.Unbox a => HasArrayOp a (U.Vector a) where
  applyArrayOp (Get _) = PL.id
  applyArrayOp (Set i x) =
    Unsafe.toLinear PL.$
      U.modify (\mv -> MU.write mv i x)
  applyArrayOp (Modify i (F.Fn f)) =
    Unsafe.toLinear PL.$
      U.modify (\mv -> MU.modify mv f i)
  applyArrayOp (Map (F.Fn f)) = Unsafe.toLinear (U.map f)

instance U.Unbox a => HasArrayOp a (LUV.Vector a) where
  applyArrayOp (Get i) = \xs -> LUV.get i xs PL.& \(Ur _, xs') -> xs'
  applyArrayOp (Set i x) = LUV.set i x
  applyArrayOp (Modify i (F.Fn f)) = LUV.modify_ f i
  applyArrayOp (Map (F.Fn f)) = PL.flip LUV.map f

applyArrayOps :: HasArrayOp a t => [ArrayOp a] -> t -> t
{-# ANN applyArrayOps "HLint: ignore Avoid lambda" #-}
applyArrayOps = flip $ foldl' (\xs op -> applyArrayOp op xs)

applyArrayOpsL :: HasArrayOp a t => [ArrayOp a] -> t %1 -> t
{-# ANN applyArrayOpsL "HLint: ignore Avoid lambda" #-}
applyArrayOpsL ops a = L.foldl' (\x (Ur o) -> applyArrayOp o x) a (map Ur ops)

checkSerialUpdateSemantics ::
  (Show a, F.Function a, Eq (v a), HasArrayOp a (v a), G.Vector v a, HasArrayOp a x, Show (v a)) =>
  F.Gen a ->
  (Linearly %1 -> [a] -> x) ->
  (x %1 -> Ur (v a)) ->
  F.Property ()
checkSerialUpdateSemantics g fromLst frz = do
  len <- F.gen $ F.integral $ F.between (1, 128)
  F.label "length" [classifyRangeBy 16 $ fromIntegral len]
  xs <- F.gen $ F.list (F.between (len, len)) g
  upds <- F.gen $ F.list (F.between (0, 20)) $ arrayOpG len g
  let (opCount, numSet, numGet, numMap, numMod, numInsts) =
        Foldl.fold
          ( (,,,,,)
              <$> Foldl.length
              <*> Foldl.prefilter (\case Set {} -> True; _ -> False) Foldl.length
              <*> Foldl.prefilter (\case Get {} -> True; _ -> False) Foldl.length
              <*> Foldl.prefilter (\case Map {} -> True; _ -> False) Foldl.length
              <*> Foldl.prefilter (\case Modify {} -> True; _ -> False) Foldl.length
              <*> Foldl.premap
                ( \case
                    Set {} -> 0 :: Int
                    Get {} -> 1
                    Map {} -> 2
                    Modify {} -> 3
                )
                (length <$> Foldl.set)
          )
          upds
  F.label "# of updates" [classifyRangeBy 10 opCount]
  F.label "# of distinct ops" [show numInsts]
  F.label "% sets" [classifyPercent numSet opCount]
  F.label "% get" [classifyPercent numGet opCount]
  F.label "% mods" [classifyPercent numMod opCount]
  F.label "% maps" [classifyPercent numMap opCount]
  F.assert $
    P.expect (applyArrayOps upds $ G.fromList xs)
      .$ ( "array"
         , unur PL.$ linearly \l ->
            frz (applyArrayOpsL upds (fromLst l xs))
         )

classifyPercent :: (Integral i) => i -> i -> String
classifyPercent _ 0 = "-"
classifyPercent i j = classifyRangeBy 10 $ fromIntegral $ 100 * i `quot` j
