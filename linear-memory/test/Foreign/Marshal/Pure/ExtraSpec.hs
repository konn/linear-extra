{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- HLINT ignore "Avoid lambda" -}
module Foreign.Marshal.Pure.ExtraSpec (test_mutation) where

import qualified Control.Functor.Linear as C
import qualified Data.Bifunctor.Linear as BiL
import Data.List (mapAccumL)
import qualified Data.List.Linear as LL
import qualified Data.Tuple.Linear as L
import Foreign.Marshal.Pure.Extra
import GHC.Generics (Generic)
import Linear.Token.Linearly.TestUtils (classifyRangeBy)
import Prelude.Linear (Ur (..), (&))
import qualified Prelude.Linear as PL
import Test.Falsify.Generator (frequency)
import qualified Test.Falsify.Generator as G
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as P
import Test.Falsify.Range (between)
import Test.Tasty (TestTree)
import Test.Tasty.Falsify

test_mutation :: TestTree
test_mutation = testProperty "works as expected" do
  i0 <- gen $ G.int (between (-10, 10))
  insts <- gen $ G.list (between (100, 1000)) instrG
  let (expFinal, expSkims) = interpret i0 insts
      Ur (actFinal, actSkims) = withPool \pool ->
        alloc i0 pool
          PL.& \ref ->
            BiL.bimap (PL.move PL.. deconstruct) unurList (runInstrs ref insts)
              PL.& \(Ur i, Ur traces) -> Ur (i, traces)
  label "# of instructions" [classifyRangeBy 100 $ length insts]
  label
    "% of sets"
    [ classifyRangeBy 10 $
        floor @Double @Int $
          100
            * fromIntegral (length $ filter (\case Set {} -> True; _ -> False) insts)
            / fromIntegral (length insts)
    ]
  label
    "% of gets"
    [ classifyRangeBy 10 $
        floor @Double @Int $
          100
            * fromIntegral (length $ filter (\case Get {} -> True; _ -> False) insts)
            / fromIntegral (length insts)
    ]
  label
    "% of modify_s"
    [ classifyRangeBy 10 $
        floor @Double @Int $
          100
            * fromIntegral (length $ filter (\case Modify {} -> True; _ -> False) insts)
            / fromIntegral (length insts)
    ]
  assert $
    P.expect expFinal .$ ("actual", actFinal)
  assert $
    P.expect expSkims .$ ("actual", actSkims)
  pure ()

unurList :: [Ur a] %1 -> Ur [a]
unurList [] = Ur []
unurList (Ur x : xs) = unurList xs & \(Ur xs) -> Ur (x : xs)

runInstrs :: Box Int %1 -> [Instr] %1 -> (Box Int, [Ur (Maybe Int)])
runInstrs = mapAccumLL interpretBox

mapAccumLL :: (s %1 -> a %1 -> (s, b)) -> s %1 -> [a] %1 -> (s, [b])
mapAccumLL f s0 xs =
  L.swap PL.$
    C.runState
      ( LL.traverse'
          (\a -> C.state \s -> L.swap (f s a))
          xs
      )
      s0

interpretBox :: Box Int %1 -> Instr %1 -> (Box Int, Ur (Maybe Int))
interpretBox !ref = \case
  Modify op -> (modify_ (runOp op) ref, Ur Nothing)
  Get -> get ref & \(Ur i, ref) -> (ref, Ur (Just i))
  Set n -> (set n ref, Ur Nothing)

interpret :: Int -> [Instr] -> (Int, [Maybe Int])
interpret = mapAccumL go
  where
    go !i = \case
      Modify op -> (runOp op i, Nothing)
      Get -> (i, Just i)
      Set n -> (n, Nothing)

runOp :: Op %1 -> Int -> Int
runOp = \case
  Add n -> (+ n)
  Sub n -> subtract n
  Mul n -> (* n)

data Op where
  Add, Sub, Mul :: !Int -> Op
  deriving (Show, Eq, Ord, Generic)

data Instr where
  Modify :: !Op -> Instr
  Get :: Instr
  Set :: !Int -> Instr
  deriving (Show, Eq, Ord, Generic)

opG :: Gen Op
opG =
  frequency
    [ (1, Add <$> G.int (between (1, 1024)))
    , (1, Sub <$> G.int (between (1, 1024)))
    , (1, Mul <$> G.int (between (1, 1024)))
    ]

instrG :: Gen Instr
instrG =
  frequency
    [ (4, Modify <$> opG)
    , (1, pure Get)
    , (2, Set <$> G.int (between (1, 1024)))
    ]
