{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Numeric.FFT.CooleyTukey.Linear (
  fft,
  fftRaw,
  reverseBit,
) where

import Data.Array.Mutable.Linear.Storable.Borrowable (RW (..), SArray, SuchThat (..))
import qualified Data.Array.Mutable.Linear.Storable.Borrowable as SA
import Data.Bits (bit, popCount, shiftR)
import Data.Complex
import Data.Function (fix)
import qualified Data.Vector.Storable as S
import GHC.Stack (HasCallStack)
import Linear.Witness.Token (besides, linearly)
import Math.NumberTheory.Logarithms (intLog2)
import Prelude.Linear hiding (Num, (*), (+))
import Prelude ((*), (+))

fft :: (HasCallStack) => S.Vector (Complex Double) -> S.Vector (Complex Double)
fft inp = unur $ linearly \l ->
  SA.fromVectorL inp l & \(SuchThat inp rw) ->
    SA.freeze (fftRaw rw inp) inp

fftRaw :: forall s. (HasCallStack) => RW s %1 -> SArray (Complex Double) s -> RW s
fftRaw (RW r w) array =
  SA.size r array & \(Ur len, r) ->
    if popCount len /= 1
      then
        SA.free (RW r w) array `lseq`
          error ("Array length must be of power of two, but got: " <> show len)
      else
        let theta = 2 * pi / fromIntegral len
         in reverseBit (RW r w) array & \rw ->
              loop rw array len (cos theta) (sin theta)
  where
    loop :: RW n %1 -> SArray (Complex Double) n -> Int -> Double -> Double -> RW n
    loop rw arr !n !c !s =
      if n <= 1
        then rw
        else
          SA.halve rw arr & \(SA.MkSlice sliced rwL rwR l r) ->
            let !half = n `quot` 2
                !dblCs = 2 * c * c - 1
                !dblSn = 2 * s * c
                !kW = c :+ s
             in loop rwL l half dblCs dblSn & \rwL ->
                  loop rwR r half dblCs dblSn & \rwR ->
                    SA.combine sliced rwL rwR l r & \(Ur _, rw) ->
                      forN
                        half
                        ( \k (RW r w) ->
                            SA.get r k arr & \(Ur ek, r) ->
                              SA.get r (half + k) arr & \(Ur ok, r) ->
                                RW r w & \rw ->
                                  SA.set rw k (ek + kW ^ k * ok) arr & \rw ->
                                    SA.set rw (half + k) (ek + kW ^ (half + k) * ok) arr
                        )
                        rw

forN :: Int -> (Int -> a %p -> a) -> a %p -> a
{-# INLINE forN #-}
{- HLINT ignore forN "Avoid lambda" -}
forN n b =
  fix
    (\self !i -> if i >= n then id else \x -> self (i + 1) (b i x))
    0

reverseBit ::
  forall s a.
  (SA.Storable a, HasCallStack) =>
  RW s %1 ->
  SArray a s ->
  RW s
reverseBit (RW r w) v =
  SA.size r v & \(Ur len, r) ->
    let !n = intLog2 len
        !m = bit $ n `shiftR` 1
     in besides (RW r w) (SA.allocL m 0)
          & \(SuchThat (table :: SArray table Int) rwTable, rw) ->
            buildTable n rwTable table & \(RW rTbl wTbl) ->
              forN
                (m - 1)
                ( \((+ 1) -> !i) (rTbl, rw) ->
                    SA.get rTbl i table & \(Ur iOff, rTbl) ->
                      forN
                        i
                        ( \j (rTbl, rw) ->
                            SA.get rTbl j table & \(Ur jOff, rTbl) ->
                              let !ji = j + iOff
                                  !ij = i + jOff
                               in SA.swap rw ji ij v & \rw ->
                                    ( rTbl
                                    , if even n
                                        then rw
                                        else SA.swap rw (ji + m) (ij + m) v
                                    )
                        )
                        (rTbl, rw)
                )
                (rTbl, rw)
                & \(rTbl, rw) ->
                  SA.free (RW rTbl wTbl) table `lseq` rw
  where
    buildTable :: Int -> RW table %1 -> SArray Int table -> RW table
    buildTable n rw table =
      fix
        ( \loop !pk !pl rw ->
            if pl + 1 >= pk
              then rw
              else
                let !k = bit $ pk - 1
                    !l = bit pl
                 in loop (pk - 1) (pl + 1) $
                      forN
                        l
                        ( \ !j (RW r w) ->
                            SA.get r j table & \(Ur t, r) ->
                              SA.set (RW r w) (l + j) (t + k) table
                        )
                        rw
        )
        n
        0
        rw
