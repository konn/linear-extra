{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Unrestricted.Linear.Orphans.Vector () where

import Data.Unrestricted.Linear
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Prelude.Linear
import qualified Unsafe.Linear as Unsafe
import Prelude hiding ((.))
import qualified Prelude as P

instance (Consumable a, U.Unbox a) => Consumable (U.Vector a) where
  consume = consume . Unsafe.toLinear U.toList
  {-# INLINE consume #-}

instance (Dupable a, U.Unbox a) => Dupable (U.Vector a) where
  dup2 = Unsafe.toLinear (U.unzip P.. U.map (forget dup2))
  {-# INLINE dup2 #-}

instance (Dupable a) => Dupable (V.Vector a) where
  dup2 = Unsafe.toLinear (V.unzip P.. V.map (forget dup2))
  {-# INLINE dup2 #-}

instance (Movable a, U.Unbox a) => Movable (U.Vector a) where
  move = Unsafe.toLinear (U.mapM (forget move))
  {-# INLINE move #-}

instance (Movable a) => Movable (V.Vector a) where
  move = Unsafe.toLinear (V.mapM (forget move))
  {-# INLINE move #-}
