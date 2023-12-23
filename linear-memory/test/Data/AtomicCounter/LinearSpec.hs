{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.AtomicCounter.LinearSpec (test_AtomicCounter) where

import Control.Concurrent (threadDelay)
import Control.Parallel.Linear (par)
import Data.AtomicCounter.Linear
import qualified Data.Bifunctor.Linear as BiL
import Data.Foldable (foldMap')
import qualified Data.Tuple.Linear as TL
import GHC.Generics (Generic)
import Linear.Witness.Token (linearly)
import Prelude.Linear (Sum (..), Ur (..), (&))
import qualified Prelude.Linear as PL
import System.IO.Unsafe (unsafePerformIO)
import Test.Falsify.Generator (frequency)
import qualified Test.Falsify.Generator as G
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as P
import Test.Falsify.Range (between)
import Test.Tasty
import Test.Tasty.Falsify

data Instruction = Wait !Int | Inc | Dec
  deriving (Show, Eq, Ord, Generic)

delayed :: Int -> a %1 -> a
{-# NOINLINE delayed #-}
delayed n a =
  unsafePerformIO (threadDelay n) `PL.seq` a

instG :: Gen Instruction
instG =
  frequency
    [ (2, pure Inc)
    , (2, pure Dec)
    , (1, Wait . fromIntegral <$> G.integral @Word (between (1, 1000)))
    ]

test_AtomicCounter :: TestTree
test_AtomicCounter =
  testGroup
    "works correctly"
    [ testProperty "Single thread" do
        thread1 <- gen $ G.list (between (0, 50)) instG
        let ths = [thread1]
            resl = withCounter ths \c1 ->
              runThread thread1 c1
                PL.& \c1 -> TL.fst (getCount c1)
        assert $ P.expect (semantics ths) .$ ("actual", resl)
    , testProperty "Two threads" do
        thread1 <- genWith (\a -> Just $ "Thread #1: " <> show a) $ G.list (between (0, 50)) instG
        thread2 <- genWith (\a -> Just $ "Thread #2: " <> show a) $ G.list (between (0, 50)) instG
        let ths = [thread1, thread2]
            (ans1, ans2) = withCounter ths \c1 ->
              PL.dup c1 & \(c1, c2) ->
                BiL.bimap
                  (TL.fst PL.. getCount)
                  (TL.fst PL.. getCount)
                  ( par
                      (runThread thread1 c1)
                      (runThread thread2 c2)
                  )
                  PL.& \(Ur c1, Ur c2) -> Ur (c1, c2)
        let expected = semantics ths
        assert $
          P.expect (expected, expected) .$ ("thread (#1, #2)", (ans1, ans2))
    , testProperty "Four threads" do
        thread1 <- genWith (\a -> Just $ "Thread #1: " <> show a) $ G.list (between (0, 50)) instG
        thread2 <- genWith (\a -> Just $ "Thread #2: " <> show a) $ G.list (between (0, 50)) instG
        thread3 <- genWith (\a -> Just $ "Thread #2: " <> show a) $ G.list (between (0, 50)) instG
        thread4 <- genWith (\a -> Just $ "Thread #4: " <> show a) $ G.list (between (0, 50)) instG
        let ths = [thread1, thread2, thread3, thread4]
            anss = withCounter ths \c1 ->
              BiL.bimap PL.dup PL.dup (PL.dup c1) & \((c1, c2), (c3, c4)) ->
                BiL.bimap
                  ( BiL.bimap
                      (TL.fst PL.. getCount)
                      (TL.fst PL.. getCount)
                  )
                  ( BiL.bimap
                      (TL.fst PL.. getCount)
                      (TL.fst PL.. getCount)
                  )
                  ( par
                      (runThread thread1 c1)
                      (runThread thread2 c2)
                      `par` par
                        (runThread thread3 c3)
                        (runThread thread4 c4)
                  )
                  PL.& \((Ur c1, Ur c2), (Ur c3, Ur c4)) -> Ur (c1, c2, c3, c4)
        let expected = semantics ths
        assert $
          P.expect (expected, expected, expected, expected) .$ ("threads", anss)
    ]

withCounter :: [[Instruction]] -> (Counter %1 -> Ur a) %1 -> a
withCounter instrs f =
  let off = calcOffs instrs
   in PL.unur PL.$ linearly PL.$ f PL.. newCounterWith off

calcOffs :: [[Instruction]] -> Word
calcOffs = getSum . foldMap' (Sum . fromIntegral . length . filter (== Dec))

semantics :: [[Instruction]] -> Word
semantics =
  fromIntegral @Int
    . getSum
    . uncurry (+)
    . foldMap'
      ( foldMap' \case
          Wait {} -> mempty
          Inc -> (1, 0)
          Dec -> (-1, 1)
      )

runThread :: [Instruction] -> Counter %1 -> Counter
runThread [] c = c
runThread (Inc : is) c =
  runThread is (increment_ c)
runThread (Dec : is) c =
  runThread is (decrement_ c)
runThread (Wait i : is) c =
  delayed i (runThread is c)
