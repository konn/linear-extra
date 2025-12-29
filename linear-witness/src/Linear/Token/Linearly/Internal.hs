{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Linear.Token.Linearly.Internal (Linearly (..)) where

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
  consume = \case Linearly -> ()
  {-# NOINLINE consume #-}

instance Dupable Linearly where
  dup2 = Unsafe.toLinear (const (Linearly, Linearly))
  {-# NOINLINE dup2 #-}
