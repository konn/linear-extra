{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}

module Data.HashMap.Mutable.Linear.WitnessSpec (test_doubleAlloc) where

import qualified Data.Bifunctor as Bi
import Data.Foldable (foldl')
import qualified Data.HashMap.Mutable.Linear as LHM
import qualified Data.HashMap.Mutable.Linear.Witness as LHM
import qualified Data.List.Linear as L
import qualified Data.Map.Strict as Map
import Data.Unrestricted.Linear (Ur (..), unur)
import Linear.Token.Linearly (besides, linearly)
import Linear.Token.Linearly.TestUtils
import Prelude.Linear ((&))
import qualified Test.Falsify.Generator as F
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as F
import Test.Tasty
import Test.Tasty.Falsify
import qualified Test.Tasty.Falsify as F

test_doubleAlloc :: TestTree
test_doubleAlloc =
  testGroup
    "can be allocated inside linearly twice"
    [ testGroup
        "fromListL"
        [ testProperty "(Int, Double)" do
            let intG = F.int $ F.between (-100, 100)
            xs <- F.gen $ F.list (F.between (0, 16)) ((,) <$> intG <*> doubleG 8)
            ys <- F.gen $ F.list (F.between (0, 16)) ((,) <$> doubleG 8 <*> intG)
            F.assert $
              P.expect (Map.fromList xs, Map.fromList ys)
                .$ ( "actual"
                   , Bi.bimap Map.fromList Map.fromList $
                      unur
                        ( linearly \l ->
                            besides l (LHM.fromListL xs) & \(xs', l') ->
                              LHM.fromListL ys l' & \ys' ->
                                distribUr (LHM.toList xs', LHM.toList ys')
                        )
                   )
        ]
    , testGroup
        "allocL"
        [ testProperty "(Int, Double)" do
            F.assert $
              P.expect ([] :: [(Int, Double)], [] :: [(Double, Int)])
                .$ ( "actual"
                   , unur
                      ( linearly \l ->
                          besides l (LHM.emptyL 256) & \(xs', l') ->
                            LHM.emptyL 256 l' & \ys' ->
                              distribUr (LHM.toList xs', LHM.toList ys')
                      )
                   )
        ]
    , testGroup
        "allocL + inserts"
        [ testProperty "(Int, Double)" do
            let intG = F.int $ F.between (-100, 100)
            xs <- F.gen $ F.list (F.between (0, 16)) ((,) <$> intG <*> doubleG 8)
            ys <- F.gen $ F.list (F.between (0, 16)) ((,) <$> doubleG 8 <*> intG)
            F.assert $
              P.expect
                ( foldl' (flip $ uncurry Map.insert) mempty xs
                , foldl' (flip $ uncurry Map.insert) mempty ys
                )
                .$ ( "actual"
                   , Bi.bimap Map.fromList Map.fromList $
                      unur
                        ( linearly \l ->
                            besides l (LHM.emptyL 256) & \(xs', l') ->
                              LHM.emptyL 256 l' & \ys' ->
                                distribUr
                                  ( LHM.toList
                                      ( L.foldl'
                                          ( \dic (Ur k, Ur v) ->
                                              LHM.insert k v dic
                                          )
                                          xs'
                                          (map (Bi.bimap Ur Ur) xs)
                                      )
                                  , LHM.toList
                                      ( L.foldl'
                                          ( \dic (Ur k, Ur v) ->
                                              LHM.insert k v dic
                                          )
                                          ys'
                                          (map (Bi.bimap Ur Ur) ys)
                                      )
                                  )
                        )
                   )
        ]
    ]