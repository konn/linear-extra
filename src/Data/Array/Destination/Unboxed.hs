{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Array.Destination.Unboxed (
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
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Exts
import GHC.Stack (HasCallStack)
import Prelude.Linear hiding (replicate)
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

data DArray a where
  DArray :: U.MVector RealWorld a -> DArray a

replicate :: U.Unbox b => b -> DArray b %1 -> ()
replicate a = fromFunction (const a)

alloc :: U.Unbox a => Int -> (DArray a %1 -> ()) %1 -> U.Vector a
alloc n writer = (\(Ur dest, vec) -> writer (DArray dest) `lseq` vec) $
  unsafeDupablePerformIO do
    destArray <- MU.unsafeNew n
    vec <- U.unsafeFreeze destArray
    P.return (Ur destArray, vec)

size :: MU.Unbox a => DArray a %1 -> (Ur Int, DArray a)
size (DArray mvec) = (Ur (MU.length mvec), DArray mvec)

fromFunction :: U.Unbox b => (Int -> b) -> DArray b %1 -> ()
fromFunction f (DArray mvec) = unsafeDupablePerformIO P.$ do
  let n = MU.length mvec
  P.sequence_ [MU.unsafeWrite mvec m (f m) | m <- [0 .. n - 1]]

fill :: (HasCallStack, U.Unbox a) => a %1 -> DArray a %1 -> ()
fill a (DArray mvec) =
  if MU.length mvec /= 1
    then error "Destination.fill: requires a destination of size 1" a
    else
      a
        & Unsafe.toLinear (unsafeDupablePerformIO P.. MU.write mvec 0)

split :: U.Unbox a => Int -> DArray a %1 -> (DArray a, DArray a)
split n (DArray mvec)
  | (ml, mr) <- MU.splitAt n mvec =
      (DArray ml, DArray mr)

mirror :: (HasCallStack, G.Vector v a, U.Unbox b) => v a -> (a %1 -> b) -> DArray b %1 -> ()
mirror v f arr =
  size arr & \(Ur sz, arr') ->
    if G.length v < sz
      then error "Destination.mirror: argument smaller than DArray" arr'
      else fromFunction (\t -> f (v G.! t)) arr'

dropEmpty :: (HasCallStack, U.Unbox a) => DArray a %1 -> ()
dropEmpty (DArray mvec)
  | MU.length mvec > 0 = error "Destination.dropEmpty on non-empty array."
  | otherwise = mvec `P.seq` ()
