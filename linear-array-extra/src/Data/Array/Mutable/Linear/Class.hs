{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Array.Mutable.Linear.Class (
  Array (..),
  unsafeAllocBeside,
  defaultUnsafeAlloc,
) where

import qualified Data.Array.Mutable.Linear.Extra as LB
import Linear.Token.Linearly
import Linear.Token.Linearly.Unsafe (HasLinearWitness)
import Prelude.Linear
import qualified Prelude as P

class (HasLinearWitness (arr a), Dupable (arr a)) => Array arr a where
  size :: arr a %1 -> (Ur Int, arr a)
  fromList :: [a] -> (arr a %1 -> Ur b) %1 -> Ur b
  fromList xs f =
    let len = P.length xs
     in unsafeAlloc len (f . go 0 xs)
    where
      go :: Int -> [a] -> arr a %1 -> arr a
      go !_ [] arr = arr
      go !i (x : xs) arr =
        go (i + 1) xs (unsafeSet i x arr)
  fromListL :: [a] -> Linearly %1 -> arr a
  fromListL xs l =
    let len = P.length xs
     in go 0 xs $ unsafeAllocL len l
    where
      go :: Int -> [a] -> arr a %1 -> arr a
      go !_ [] arr = arr
      go !i (x : xs) arr =
        go (i + 1) xs (unsafeSet i x arr)
  fill :: a -> arr a %1 -> arr a
  fill a arr = size arr & \(Ur sz, arr) -> go 0 sz arr
    where
      go :: Int -> Int -> arr a %1 -> arr a
      go !i !sz arr
        | i P.>= sz = arr
        | otherwise = go (i + 1) sz (unsafeSet i a arr)
  unsafeAlloc :: Int -> (arr a %1 -> Ur b) %1 -> Ur b
  unsafeAlloc = defaultUnsafeAlloc
  unsafeAllocL :: Int -> Linearly %1 -> arr a
  unsafeSet :: Int -> a -> arr a %1 -> arr a
  unsafeGet :: Int -> arr a %1 -> (Ur a, arr a)
  unsafeSlice :: Int -> Int -> arr a %1 -> (arr a, arr a)
  unsafeResize :: Int -> arr a %1 -> arr a

defaultUnsafeAlloc :: (Array arr a) => Int -> (arr a %1 -> Ur b) %1 -> Ur b
defaultUnsafeAlloc i f = linearly $ f . unsafeAllocL i

unsafeAllocBeside ::
  (Array arr a, HasLinearWitness wit) =>
  Int ->
  wit %1 ->
  (arr a, wit)
unsafeAllocBeside sz wit = besides wit (unsafeAllocL sz)

instance Array LB.Array a where
  size = LB.size
  fromList :: [a] -> (LB.Array a %1 -> Ur b) %1 -> Ur b
  fromList = LB.fromList
  unsafeAlloc sz = LB.alloc sz (error "Uninitialised element")
  unsafeAllocL sz = LB.allocL sz (error "Uninitialised element")
  unsafeSet = LB.set
  unsafeGet = LB.get
  unsafeSlice = LB.slice
  unsafeResize i = LB.resize i (error "Uninitialised element")
