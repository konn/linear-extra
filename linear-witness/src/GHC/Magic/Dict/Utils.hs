{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GHC.Magic.Dict.Utils (withDictL) where

import GHC.Magic.Dict.Compat
import qualified Unsafe.Linear as Unsafe

newtype RequiresDict cls r = RequiresDict ((cls) => r)

withDictL :: forall cls meth r. (WithDict cls meth) => meth -> ((cls) => r) %1 -> r
withDictL meth k =
  Unsafe.toLinear
    (\(RequiresDict f) -> withDict @cls @meth meth f)
    (RequiresDict @cls @r k)