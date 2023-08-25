{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Alloc.Linearly.Token.Unsafe (HasLinearWitness, linearWitness) where

import Data.Alloc.Linearly.Token.Internal
import qualified Data.Array.Mutable.Linear as MA
import qualified Data.HashMap.Mutable.Linear as LHM
import qualified Data.Set.Mutable.Linear as LSet
import Data.Unrestricted.Linear (dup2)
import qualified Data.Vector.Mutable.Linear as MV
import qualified Unsafe.Linear as Unsafe

class HasLinearWitness a where
  linearWitness_ :: a %1 -> (a, Linearly)
  linearWitness_ = Unsafe.toLinear (,Linearly)

linearWitness :: HasLinearWitness a => a %1 -> (a, Linearly)
{-# NOINLINE linearWitness #-}
linearWitness = linearWitness_

instance HasLinearWitness Linearly where
  linearWitness_ = dup2

deriving anyclass instance HasLinearWitness (MA.Array a)

deriving anyclass instance HasLinearWitness (MV.Vector a)

deriving anyclass instance HasLinearWitness (LHM.HashMap k a)

deriving anyclass instance HasLinearWitness (LSet.Set a)
