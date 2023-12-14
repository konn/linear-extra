{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.AtomicCounter.LinearSpec (test_AtomicCounter) where

import Control.Concurrent (threadDelay)
import Data.AtomicCounter.Linear
import Data.Foldable (foldMap')
import qualified Data.Tuple.Linear as TL
import Foreign.Marshal.Pure.Extra (withPool)
import GHC.Generics (Generic)
import Prelude.Linear (Sum (..), Ur (..))
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
    , (1, Wait . fromIntegral <$> G.integral @Word (between (1, 10 ^ (3 :: Int))))
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
    ]

withCounter :: [[Instruction]] -> (Counter %1 -> Ur a) %1 -> a
withCounter instrs f =
  let off = calcOffs instrs
   in PL.unur PL.$ withPool PL.$ f PL.. newCounterWith off

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
