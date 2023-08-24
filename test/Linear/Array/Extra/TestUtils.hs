{-# LANGUAGE LinearTypes #-}

module Linear.Array.Extra.TestUtils (classifyRangeBy, fst', snd', doubleG) where

import Data.Functor ((<&>))
import qualified Prelude.Linear as PL
import qualified Test.Falsify.Generator as F
import qualified Test.Falsify.Range as F

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
