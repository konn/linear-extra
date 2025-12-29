{-# LANGUAGE DeriveGeneric #-}

module Numeric.FFT.App.Csv where

import qualified Data.Csv as Csv
import Data.Reflection
import GHC.Generics (Generic)
import qualified Streaming.Prelude as S

data Newline = CRLF | LF | CR
  deriving (Show, Eq, Ord, Generic)

data Options = Options {input :: !FilePath, newline :: !Newline}
  deriving (Show, Eq, Ord, Generic)
