{-# LANGUAGE BlockArguments #-}

module Numeric.FFT.CooleyTukey.LinearSpec (test_fftPar) where

import Data.Complex
import Data.Functor ((<&>))
import qualified Data.Vector.Storable as S
import Numeric.FFT.CooleyTukey.Linear
import qualified Test.Falsify.Generator as F
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as F
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Tasty.Falsify as F

doubleG :: F.Precision -> F.Gen Double
doubleG i = F.properFraction i <&> \(F.ProperFraction d) -> d

sample :: Int -> (Double -> Double) -> S.Vector (Complex Double)
sample n f =
  S.generate n (\i -> f (-4 + 8 * fromIntegral i / fromIntegral n) :+ 0.0)

fun :: Double -> Double
fun x = sin (2 * pi * x) + 2 * sin (pi * x) + 3 * sin (0.5 * pi * x)

test_fftPar :: TestTree
test_fftPar =
  testGroup
    "fftPar"
    [ testProperty "fftPar === fft" do
        vec <- F.gen do
          n <- F.int $ F.between (4, 10)
          pure $ sample (2 ^ n) fun
        thresh <- F.gen $ F.int $ F.between (1, S.length vec)
        F.assert $
          P.expect (fft vec)
            .$ ("parallel", fftPar thresh vec)
    ]
