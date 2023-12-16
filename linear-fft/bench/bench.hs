{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Control.Concurrent (setNumCapabilities)
import Control.DeepSeq
import Control.Exception
import Data.Complex
import qualified Data.Vector.Storable as S
import qualified Numeric.FFT.CooleyTukey.Linear as FFT
import Test.Tasty (localOption)
import Test.Tasty.Bench
import Test.Tasty.Runners (NumThreads (NumThreads))

numWorkers :: [Int]
numWorkers = [1, 2, 4, 8, 16, 32, 64, 128, 256]

main :: IO ()
main =
  defaultMain
    [ localOption (NumThreads 1) $
        localOption WallTime scalingBench
    ]

fun :: Double -> Double
fun x = sin (2 * pi * x) + 2 * sin (pi * x) + 3 * sin (0.5 * pi * x)

sample :: Int -> (Double -> Double) -> S.Vector (Complex Double)
sample n f =
  S.generate n (\i -> f (-4 + 8 * fromIntegral i / fromIntegral n) :+ 0.0)

scalingBench :: Benchmark
scalingBench = bgroup "scaling" [waeakScalingBench, strongScalingBench]

waeakScalingBench :: Benchmark
waeakScalingBench =
  bgroup
    "Weak scaling"
    [ bgroup (show size <> "/job") $
      [ env (setNumCapabilities n >> evaluate (force $ sample len fun)) $ \v ->
        bench (show n) $ nf (FFT.fftPar size) v
      | n <- numWorkers
      , let !len = n * size
      ]
    | size <- jobSizes
    ]
  where
    jobSizes = [256, 512, 1024, 2048, 4096, 8192]

strongScalingBench :: Benchmark
strongScalingBench =
  bgroup
    "Strong sclaing"
    [ env (evaluate (force $ sample size fun)) $ \v ->
      bgroup
        ("size = " <> show size)
        [ env (setNumCapabilities caps) \_ ->
          bench (show caps) $ nf (FFT.fftPar (size `quot` caps)) v
        | caps <- numWorkers
        ]
    | base <- totalSizeBase
    , let !size = 2 ^ base
    ]
  where
    totalSizeBase = [18 .. 21 :: Int]
