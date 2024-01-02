{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
Utility module to throw away with '*Beseide' constraints, as described in
<https://www.tweag.io/blog/2023-03-23-linear-constraints-linearly/ LINEAR CONSTRAINTS: THE PROBLEM WITH SCOPES>.
Thisis just a tentative workaround until Linearly Qualified Types gets implemented in GHC.
-}
module Linear.Token.Linearly (Linearly (), linearly, besides) where

import Data.Unrestricted.Linear
import GHC.Base (noinline)
import Linear.Token.Linearly.Internal
import Linear.Token.Linearly.Unsafe (HasLinearWitness, linearWitness)
import Prelude.Linear ((&))

linearly :: (Linearly %1 -> Ur a) %1 -> Ur a
{-# NOINLINE linearly #-}
linearly = noinline \k -> k (noinline Linearly)

besides :: (HasLinearWitness a) => a %1 -> (Linearly %1 -> b) %1 -> (b, a)
-- NOTE: For some (unclear) reasons, those NOINLINE/noinline are needed
-- to prevent the internal state float out when called more than twice
{-# NOINLINE besides #-}
besides = noinline \wit f ->
  linearWitness wit & \(wit, lin) ->
    (f lin, wit)
