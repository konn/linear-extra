{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Array.Polarized.Pull.ExtraSpec (test_fromVector) where

import qualified Data.Array.Polarized as PA
import qualified Data.Array.Polarized.Pull.Extra as Pull
import qualified Data.Array.Polarized.Push.Extra as Push
import qualified Data.DList as DL
import Data.Unrestricted.Linear (unur)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Linear.Array.Extra.TestUtils
import Prelude.Linear (Ur (..))
import qualified Prelude.Linear as PL
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as P
import Test.Tasty
import Test.Tasty.Falsify
import qualified Test.Tasty.Falsify as F

test_fromVector :: TestTree
test_fromVector =
  testGroup
    "fromVector"
    [ testGroup
        "Push.foldMap DL.singleton . transfer . Pull.fromVector = id"
        [ testWithGens "Boxed Vector" $ checkFromVectorToDL @V.Vector
        , testWithGens "Unboxed Vector" $ checkFromVectorToDL @U.Vector
        ]
    ]

checkFromVectorToDL ::
  forall v a.
  (G.Vector v a, Show a, Eq a) =>
  Gen a ->
  Property ()
checkFromVectorToDL g = do
  (_l, xs) <- genLenList g
  let v = G.fromList @v @a xs
  F.assert $
    P.expect xs
      .$ ( "actual"
         , DL.toList $
            unur PL.$
              Push.foldMap (Ur . DL.singleton) (PA.transfer (Pull.fromVector v))
         )
