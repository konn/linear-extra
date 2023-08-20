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

import Data.Unrestricted.Linear
import GHC.Generics (Generic)
import Generics.Linear.TH (deriveGeneric)
import Prelude.Linear
import qualified Prelude.Linear.Generically as L
import qualified Prelude as P

data Linearly = Linearly
  deriving (Show, P.Eq, P.Ord, Generic)

deriveGeneric ''Linearly

deriving via AsMovable Linearly instance Consumable Linearly

deriving via AsMovable Linearly instance Dupable Linearly

deriving via L.Generically Linearly instance Movable Linearly
