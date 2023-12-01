{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Numeric.FFT.App (defaultMain) where

import Control.Applicative ((<**>))
import Control.DeepSeq (force, rnf)
import Control.Exception (evaluate)
import Data.Complex
import qualified Data.FMList as FML
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import qualified Numeric.FFT.CooleyTukey.Linear as FFT
import qualified Options.Applicative as Opts
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

data Opts = Opts
  { threshold :: !Int
  , sequential :: !Bool
  , size :: !Int
  , output :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Ord)

optionsP :: Opts.ParserInfo Opts
optionsP = Opts.info (p <**> Opts.helper) $ Opts.progDesc "Parallel FFT"
  where
    p = do
      threshold <-
        Opts.option Opts.auto $
          Opts.short 'n'
            <> Opts.long "threshold"
            <> Opts.value 1024
            <> Opts.showDefault
            <> Opts.help "Threshold to calculate sequentially below this length."
      size <-
        Opts.option Opts.auto $
          Opts.short 'N'
            <> Opts.long "size"
            <> Opts.value kN
            <> Opts.showDefault
            <> Opts.help "Size"
      output <-
        Opts.optional $
          Opts.strOption $
            Opts.short 'o'
              <> Opts.metavar "FILE"
              <> Opts.help "Output TSV path"
      sequential <- Opts.switch $ Opts.short 's' <> Opts.long "seq" <> Opts.help "Sequential FFT"
      pure Opts {..}

sample :: Int -> (Double -> Double) -> S.Vector (Complex Double)
sample n f =
  S.generate n (\i -> f (-4 + 8 * fromIntegral i / fromIntegral n) :+ 0.0)

kN :: Int
kN = 2 ^ (20 :: Int)

fun :: Double -> Double
fun x = sin (2 * pi * x) + 2 * sin (pi * x) + 3 * sin (0.5 * pi * x)

defaultMain :: IO ()
defaultMain = do
  Opts {..} <- Opts.execParser optionsP
  !v <- evaluate $ force $ sample size fun
  let fft
        | sequential = FFT.fft
        | otherwise = FFT.fftPar threshold
      retrv = case output of
        Nothing -> evaluate . rnf
        Just fp -> \vs -> do
          createDirectoryIfMissing True $ takeDirectory fp
          writeFile fp
            $ unlines
            $ FML.toList
            $ U.foldMap
              (\(i, c) -> FML.singleton $ show i <> "\t" <> show (magnitude c))
            $ U.indexed
            $ S.convert vs
          putStrLn $ "Written to: " <> fp
  retrv $ fft v
