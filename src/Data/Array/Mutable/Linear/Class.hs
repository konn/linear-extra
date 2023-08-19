{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Array.Mutable.Linear.Class (Array (..)) where

import qualified Data.Array.Mutable.Linear as LB
import Prelude.Linear
import qualified Prelude as P

class Dupable (arr a) => Array arr a where
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
  fill :: a -> arr a %1 -> arr a
  fill a arr = size arr & \(Ur sz, arr) -> go 0 sz arr
    where
      go :: Int -> Int -> arr a %1 -> arr a
      go !i !sz arr
        | i P.>= sz = arr
        | otherwise = go (i + 1) sz (unsafeSet i a arr)
  unsafeAlloc :: Int -> (arr a %1 -> Ur b) %1 -> Ur b
  unsafeAllocBeside :: Int -> arr b %1 -> (arr a, arr b)
  unsafeSet :: Int -> a -> arr a %1 -> arr a
  unsafeGet :: Int -> arr a %1 -> (Ur a, arr a)
  unsafeSlice :: Int -> Int -> arr a %1 -> (arr a, arr a)
  unsafeResize :: Int -> arr a %1 -> arr a

instance Array LB.Array a where
  size = LB.size
  fromList :: [a] -> (LB.Array a %1 -> Ur b) %1 -> Ur b
  fromList = LB.fromList
  unsafeAlloc sz = LB.alloc sz (error "Uninitialised element")
  unsafeAllocBeside sz = LB.allocBeside sz (error "Uninitialised element")
  unsafeSet = LB.set
  unsafeGet = LB.get
  unsafeSlice = LB.slice
  unsafeResize i = LB.resize i (error "Uninitialised element")
