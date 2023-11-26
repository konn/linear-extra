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
  reverseBit,
) where

import Data.Array.Mutable.Linear.Storable.Borrowable (RW (..), SArray, SuchThat (..))
import qualified Data.Array.Mutable.Linear.Storable.Borrowable as SA
import Data.Bits (bit, shiftR)
import Data.Function (fix)
import GHC.Stack (HasCallStack)
import Linear.Witness.Token (besides)
import Math.NumberTheory.Logarithms (intLog2)
import Prelude.Linear

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
