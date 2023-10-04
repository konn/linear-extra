{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Linear.Witness.Token.Unsafe (HasLinearWitness, linearWitness, linearWitness#) where

import qualified Data.Array.Destination as DA
import qualified Data.Array.Mutable.Linear as MA
import qualified Data.Array.Mutable.Unlifted.Linear as MAU
import qualified Data.Array.Polarized.Pull as Pull
import qualified Data.Array.Polarized.Push as Push
import Data.Either (Either)
import qualified Data.HashMap.Mutable.Linear as LHM
import Data.Maybe (Maybe)
import qualified Data.Set.Mutable.Linear as LSet
import Data.Unrestricted.Linear (Ur)
import Data.V.Linear (V)
import qualified Data.Vector.Mutable.Linear as MV
import Foreign.Marshal.Pure
import GHC.Exts
import GHC.TypeLits
import Linear.Witness.Token.Internal
import qualified Streaming.Linear as S
import qualified System.IO.Linear as L
import System.IO.Resource.Linear (RIO)

type HasLinearWitness :: TYPE rep -> Constraint
class HasLinearWitness a

linearWitness :: (HasLinearWitness a) => a %1 -> (a, Linearly)
{-# NOINLINE linearWitness #-}
linearWitness = noinline (,Linearly)

linearWitness# :: forall (a :: TYPE UnliftedRep). (HasLinearWitness a) => a %1 -> (# a, Linearly #)
{-# NOINLINE linearWitness# #-}
linearWitness# = noinline (# ,Linearly #)

deriving anyclass instance HasLinearWitness Linearly

deriving anyclass instance HasLinearWitness (MA.Array a)

deriving anyclass instance HasLinearWitness (MAU.Array# a)

deriving anyclass instance HasLinearWitness (DA.DArray a)

deriving anyclass instance HasLinearWitness (MV.Vector a)

deriving anyclass instance HasLinearWitness (LHM.HashMap k a)

deriving anyclass instance HasLinearWitness (LSet.Set a)

deriving anyclass instance HasLinearWitness Pool

deriving anyclass instance HasLinearWitness (Box a)

-- Non-instances
instance (TypeError ('Text "Pull Array cannot have linear witness (fromFunction and singleton violate the invariant)")) => HasLinearWitness (Pull.Array a)

instance (TypeError ('Text "Push Array cannot have linear witness (make and singleton violate the invariant)")) => HasLinearWitness (Push.Array a)

instance
  ( TypeError ('ShowType (V n a) ':<>: 'Text " cannot have linear witness (empty violates the invariant)")
  ) =>
  HasLinearWitness (V n a)

instance
  ( TypeError ('ShowType (S.Stream f m a) ':<>: 'Text " cannot have linear witness (delay violates the invariant)")
  ) =>
  HasLinearWitness (S.Stream f m a)

instance
  ( TypeError ('ShowType [a] ':<>: 'Text " cannot have linear witness (pure violates the invariant)")
  ) =>
  HasLinearWitness [a]

instance
  ( TypeError ('ShowType (Ur a) ':<>: 'Text " cannot have linear witness (pure violates the invariant)")
  ) =>
  HasLinearWitness (Ur a)

instance
  ( TypeError ('ShowType (Maybe a) ':<>: 'Text " cannot have linear witness (pure violates the invariant)")
  ) =>
  HasLinearWitness (Maybe a)

instance
  ( TypeError ('ShowType (Either a b) ':<>: 'Text " cannot have linear witness (pure violates the invariant)")
  ) =>
  HasLinearWitness (Either a b)

instance
  ( TypeError ('ShowType (L.IO a) ':<>: 'Text " cannot have linear witness (pure violates the invariant)")
  ) =>
  HasLinearWitness (L.IO a)

instance
  ( TypeError ('ShowType (RIO a) ':<>: 'Text " cannot have linear witness (pure violates the invariant)")
  ) =>
  HasLinearWitness (RIO a)
