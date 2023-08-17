{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Array.Destination.Generic (
  DArray (),

  -- * Create and use a 'DArray'
  alloc,
  size,

  -- * Ways to write to a 'DArray'
  replicate,
  split,
  mirror,
  fromFunction,
  fill,
  dropEmpty,
) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import GHC.Exts
import GHC.Stack (HasCallStack)
import Prelude.Linear hiding (replicate)
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

data DArray v a where
  DArray :: G.Mutable v RealWorld a -> DArray v a

replicate :: G.Vector v b => b -> DArray v b %1 -> ()
replicate a = fromFunction (const a)

alloc :: G.Vector v a => Int -> (DArray v a %1 -> ()) %1 -> v a
alloc n writer = (\(Ur dest, vec) -> writer (DArray dest) `lseq` vec) $
  unsafeDupablePerformIO do
    destArray <- MG.unsafeNew n
    vec <- G.unsafeFreeze destArray
    P.return (Ur destArray, vec)

size :: G.Vector v a => DArray v a %1 -> (Ur Int, DArray v a)
size (DArray mvec) = (Ur (MG.length mvec), DArray mvec)

fromFunction :: G.Vector v b => (Int -> b) -> DArray v b %1 -> ()
fromFunction f (DArray mvec) = unsafeDupablePerformIO P.$ do
  let n = MG.length mvec
  P.sequence_ [MG.unsafeWrite mvec m (f m) | m <- [0 .. n - 1]]

fill :: (HasCallStack, G.Vector v a) => a %1 -> DArray v a %1 -> ()
fill a (DArray mvec) =
  if MG.length mvec /= 1
    then error "Destination.fill: requires a destination of size 1" a
    else
      a
        & Unsafe.toLinear (unsafeDupablePerformIO P.. MG.write mvec 0)

split :: G.Vector v a => Int -> DArray v a %1 -> (DArray v a, DArray v a)
split n (DArray mvec)
  | (ml, mr) <- MG.splitAt n mvec =
      (DArray ml, DArray mr)

mirror :: (HasCallStack, G.Vector u a, G.Vector v b) => u a -> (a %1 -> b) -> DArray v b %1 -> ()
mirror v f arr =
  size arr & \(Ur sz, arr') ->
    if G.length v < sz
      then error "Destination.mirror: argument smaller than DArray" arr'
      else fromFunction (\t -> f (v G.! t)) arr'

dropEmpty :: (HasCallStack, G.Vector v a) => DArray v a %1 -> ()
dropEmpty (DArray mvec)
  | MG.length mvec > 0 = error "Destination.dropEmpty on non-empty array."
  | otherwise = mvec `P.seq` ()
