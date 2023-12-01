{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

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

data Opts = Opts {threshold :: !Int, size :: !Int, output :: !(Maybe FilePath)}
  deriving (Show, Eq, Ord)

optsP :: Opts.ParserInfo Opts
optsP = Opts.info (p <**> Opts.helper) $ Opts.progDesc "Parallel FFT"
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
      pure Opts {..}

sample :: Double -> Int -> (Double -> Double) -> S.Vector (Complex Double)
sample eps n f =
  S.generate n (\i -> f (eps * fromIntegral i / fromIntegral n) :+ 0.0)

kN :: Int
kN = 2 ^ (20 :: Int)

fun :: Double -> Double
fun x = sin (2 * pi * x) + 2 * cos (pi * 0.25 * x) + 4 * sin (0.5 * pi * x - pi / 8)

main :: IO ()
main = do
  Opts {..} <- Opts.execParser optsP
  !v <- evaluate $ force $ sample 0.125 size fun
  let retrv :: S.Vector (Complex Double) -> IO ()
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
  retrv $ FFT.fftPar threshold v
