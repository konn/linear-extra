{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Alloc.Linearly.Token.Internal (Linearly (..)) where

import Control.Monad (join)
import Data.Unrestricted.Linear
import GHC.Generics (Generic)
import Generics.Linear.TH (deriveGeneric)
import Prelude.Linear
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

data Linearly = Linearly
  deriving (Show, P.Eq, P.Ord, Generic)

deriveGeneric ''Linearly

instance Consumable Linearly where
  consume = Unsafe.toLinear (const ())
  {-# INLINE consume #-}

instance Dupable Linearly where
  dup2 = Unsafe.toLinear (join (,))
