{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
Utility module to throw away with '*Beseide' constraints, as described in
<https://www.tweag.io/blog/2023-03-23-linear-constraints-linearly/ LINEAR CONSTRAINTS: THE PROBLEM WITH SCOPES>.
Thisis just a tentative workaround until Linearly Qualified Types gets implemented in GHC.
-}
module Data.Alloc.Linearly.Token (Linearly (), linearly, besides) where

import Data.Alloc.Linearly.Token.Internal
import Data.Alloc.Linearly.Token.Unsafe (HasLinearWitness, linearWitness)
import Data.Unrestricted.Linear
import Prelude.Linear ((&))

linearly :: (Linearly %1 -> Ur a) %1 -> Ur a
{-# NOINLINE linearly #-}
linearly k = k Linearly

besides :: HasLinearWitness a => a %1 -> (Linearly %1 -> b) %1 -> (b, a)
{-# NOINLINE besides #-}
besides wit f =
  linearWitness wit & \(wit, lin) ->
    (f lin, wit)
