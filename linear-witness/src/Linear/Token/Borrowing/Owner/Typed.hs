{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

module Linear.Token.Borrowing.Owner.Typed (
  Owner (),
  deconsOwner,
  consumeOwner,
  lseq,
  newOwner,
  lending,
  lendingMut,
  lend,
  lendMut,
  unlend,
  unlendMut,
  KnownLocations,
  sLocs,
  SLocs (..),
  KnownLocation (..),
  sInsert,
  sDelete,
  sMember,
  Existence (..),
) where

import Linear.Token.Borrowing (KnownLocation (..))
import Linear.Token.Borrowing.Owner.Typed.Internal
