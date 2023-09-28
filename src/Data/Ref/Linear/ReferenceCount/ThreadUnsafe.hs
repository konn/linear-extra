{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- FIXME: allocate strong/weak counter also off-heap
module Data.Ref.Linear.ReferenceCount.ThreadUnsafe (
  Rc (),
  alloc,
  deref,
  modify,
  modify_,
  toBox,
  deconstruct,
  Weak (),
  downgrade,
  upgrade,
  ReferenceCountException (..),
) where

import Control.Exception (Exception, mask_, throw)
import Data.Alloc.Linearly.Token
import Data.Alloc.Linearly.Token.Unsafe (HasLinearWitness)
import Data.Array.Mutable.Linear.Primitive (PrimArray (..))
import qualified Data.Array.Mutable.Linear.Primitive as PA
import Data.Array.Mutable.Unlifted.Linear.Primitive (PrimArray#)
import qualified Data.Array.Mutable.Unlifted.Linear.Primitive as PAU
import qualified Data.Bifunctor.Linear.Internal.Bifunctor as L
import qualified Data.Replicator.Linear as Rep
import Data.Word
import Foreign (free, peek)
import Foreign.Marshal.Pure (Box, Pool, Representable)
import qualified Foreign.Marshal.Pure as Box hiding (deconstruct)
import qualified Foreign.Marshal.Pure.Internal as Box
import GHC.Exts (runRW#)
import GHC.IO (unIO)
import Prelude.Linear
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

type RefCounter# = PrimArray# Word64

-- | Strong, Weak, and Entity
newtype RcBox a = RcBox (# RefCounter#, RefCounter#, Box a #)

mkRcBox :: RefCounter# %1 -> RefCounter# %1 -> Box a %1 -> RcBox a
{-# INLINE mkRcBox #-}
mkRcBox s w b = RcBox (# s, w, b #)

{- |
A reference-counted mutable cell, allocated off-heap.

__This is thread-unsafe__.
-}
data Rc a where
  Rc :: {-# UNPACK #-} !(RcBox a) %1 -> Rc a
  deriving anyclass (HasLinearWitness)

{- |
A weak reference to some 'Rc'.
__This is thread-unsafe__.

Use 'downgrade' and 'upgrade' to convert from/to 'Rc'.
-}
data Weak a where
  Weak :: {-# UNPACK #-} !(RcBox a) %1 -> Weak a
  deriving anyclass (HasLinearWitness)

decrCounter :: RefCounter# %1 -> (# Ur Word64, RefCounter# #)
decrCounter c = go (PAU.get 0 c)
  where
    {-# INLINE go #-}
    go :: (# Ur Word64, RefCounter# #) %1 -> (# Ur Word64, RefCounter# #)
    go (# Ur n, c #) =
      (# Ur (n P.- 1), PAU.set 0 (n P.- 1) c #)

decrCounterConsume :: RefCounter# %1 -> ()
decrCounterConsume c = go (PAU.get 0 c)
  where
    {-# INLINE go #-}
    go :: (# Ur Word64, RefCounter# #) %1 -> ()
    go (# Ur n, c #) = PAU.set 0 (n P.- 1) c `PAU.lseq` ()

instance Consumable (Rc a) where
  {-# INLINE consume #-}
  consume (Rc (RcBox (# strong, weak, b #))) = go (decrCounter strong) weak b
    where
      -- FIXME: when making strong and weak allocated off-heap
      -- consider weak count and release them accordingly
      go :: (# Ur Word64, RefCounter# #) %1 -> RefCounter# %1 -> Box a %1 -> ()
      go (# Ur 0, strong #) weak = \b ->
        freeBox b `lseq` decrWeak weak `PAU.lseq` strong `PAU.lseq` ()
      go (# Ur _, strong #) weak = Unsafe.toLinear \_ ->
        strong `PAU.lseq` weak `PAU.lseq` ()

data ReferenceCountException
  = EmptyStrongCount
  | StrongCountLimitExceed
  | EmptyWeakCount
  | WeakCountLimitExceeded
  deriving (Show, P.Eq, P.Ord)
  deriving anyclass (Exception)

instance Dupable (Rc a) where
  dupR = Unsafe.toLinear \(Rc box) ->
    case incrStrong box of
      !box -> Rep.pure (Rc box)

maxRefCount :: Word64
maxRefCount = 0x7FFFFFFFFFFFFFFF

freeBox :: Box a %1 -> ()
freeBox (Box.Box poolPtr ptr) = unsafeDupablePerformIO $ mask_ $ do
  Box.delete P.=<< peek poolPtr
  free ptr
  free poolPtr

alloc :: Representable a => a %1 -> Pool %1 -> Rc a
{-# INLINE alloc #-}
alloc a0 pool =
  besides
    pool
    ( \l ->
        dup2 l & \(l1, l2) ->
          (PA.allocL l1 1 1, PA.allocL l2 1 1)
    )
    & \((PrimArray strong, PrimArray weak), pool) ->
      Box.alloc a0 pool & \box ->
        Rc (RcBox (# strong, weak, box #))

-- | __Warning__: This is non-atomic!
modify :: Representable a => (a -> (a, b)) -> Rc a %1 -> (Ur b, Rc a)
modify f (Rc (RcBox (# strong, weak, box #))) =
  modifyBox f box & \(urb, box) ->
    (urb, Rc (RcBox (# strong, weak, box #)))

-- | __Warning__: This is non-atomic!
modify_ :: Representable a => (a -> a) -> Rc a %1 -> Rc a
modify_ f (Rc (RcBox (# strong, weak, box #))) =
  modifyBox_ f box & \box ->
    Rc (RcBox (# strong, weak, box #))

modifyBox :: Representable a => (a -> (a, b)) -> Box a %1 -> (Ur b, Box a)
modifyBox f b =
  readBox b & \(Ur a, box) ->
    (a `seq` f a) & \(!a, !b) -> (Ur b, writeBox a box)

modifyBox_ :: Representable a => (a -> a) -> Box a %1 -> Box a
modifyBox_ f b =
  readBox b & \(Ur a, box) ->
    writeBox (a `seq` f a) box

writeBox :: Representable a => a -> Box a %1 -> Box a
{-# NOINLINE writeBox #-}
writeBox !a = Unsafe.toLinear \(Box.Box poolPtr ptr) ->
  case runRW# (unIO (Box.reprPoke ptr a)) of
    (# _, () #) -> Box.Box poolPtr ptr

deref :: Representable a => Rc a %1 -> (Ur a, Rc a)
{-# INLINE deref #-}
deref (Rc (RcBox (# strong, weak, box #))) =
  readBox box & \(ura, box) ->
    (ura, Rc (RcBox (# strong, weak, box #)))

-- | Tries to deconstruct reference-coutned cell and deallocate heap, if there is only one 'Rc'.
deconstruct :: Representable a => Rc a %1 -> Either (Rc a) a
{-# INLINE deconstruct #-}
deconstruct = L.bimap id Box.deconstruct . toBox

-- | Tries to convert reference-counted reference into a Box if there is only one 'Rc'.
toBox :: Rc a %1 -> Either (Rc a) (Box a)
{-# INLINE toBox #-}
toBox (Rc (RcBox (# strong, weak, box #)) :: Rc a) =
  go (PAU.get 0 strong) weak box
  where
    {-# INLINE go #-}
    go :: (# Ur Word64, RefCounter# #) %1 -> RefCounter# %1 -> Box a %1 -> Either (Rc a) (Box a)
    go (# Ur 1, strong #) weak box =
      decrCounterConsume strong `lseq` decrCounterConsume weak `lseq` Right box
    go (# Ur _, strong #) weak box =
      Left (Rc (RcBox (# strong, weak, box #)))

readBox :: Representable a => Box a %1 -> (Ur a, Box a)
{-# NOINLINE readBox #-}
readBox = Unsafe.toLinear \box@(Box.Box _ b) ->
  case runRW# (unIO (Box.reprPeek b)) of
    (# _, a #) -> (Ur a, box)

incrStrong :: forall a. RcBox a %1 -> RcBox a
incrStrong (RcBox (# strong, weak, box #)) = incr (PAU.get 0 strong) weak box
  where
    incr :: (# Ur Word64, RefCounter# #) %1 -> RefCounter# %1 -> Box a %1 -> RcBox a
    {-# INLINE incr #-}
    incr (# Ur n, strong #) weak box =
      n & \case
        0 ->
          consume (Rc (mkRcBox strong weak box)) & \case
            () -> throw EmptyWeakCount
        n ->
          (n P.> maxRefCount) & \case
            True ->
              consume (Rc (mkRcBox strong weak box)) & \case
                () -> throw WeakCountLimitExceeded
            False -> mkRcBox (PAU.set 0 (n P.+ 1) strong) weak box

incrWeak :: forall a. RcBox a %1 -> RcBox a
incrWeak (RcBox (# strong, weak, box #)) = incr strong box (PAU.get 0 weak)
  where
    incr :: RefCounter# %1 -> Box a %1 -> (# Ur Word64, RefCounter# #) %1 -> RcBox a
    {-# INLINE incr #-}
    incr strong box (# Ur n, weak #) =
      n & \case
        0 ->
          consume (Rc (mkRcBox strong weak box)) & \case
            () -> throw EmptyWeakCount
        n ->
          (n P.> maxRefCount) & \case
            True ->
              consume (Rc (mkRcBox strong weak box)) & \case
                () -> throw WeakCountLimitExceeded
            False -> mkRcBox strong (PAU.set 0 (n P.+ 1) weak) box

decrWeak :: RefCounter# %1 -> RefCounter#
decrWeak weak =
  let dec :: (# Ur Word64, RefCounter# #) %1 -> RefCounter#
      dec (# Ur n, weak #) = PAU.set 0 (n P.- 1) weak
   in dec (PAU.get 0 weak)

downgrade :: Rc a %1 -> (Weak a, Rc a)
downgrade = Unsafe.toLinear \(Rc box) ->
  case incrWeak box of
    box -> (Weak box, Rc box)

instance Consumable (Weak a) where
  -- FIXME: when making strong and weak allocated off-heap
  -- consider weak count and release them accordingly
  consume = Unsafe.toLinear \(Weak (RcBox (# _strong, weak, _b #))) ->
    decrWeak weak `PAU.lseq` ()

instance Dupable (Weak a) where
  dupR = Unsafe.toLinear \(Weak box) ->
    case incrWeak box of
      box -> Rep.pure (Weak box)

upgrade :: Weak a %1 -> (Maybe (Rc a), Weak a)
upgrade = Unsafe.toLinear \(Weak (RcBox (# strong, weak, box #))) ->
  case PAU.get 0 strong of
    (# Ur n, strong #) ->
      case n of
        0 ->
          -- Strong counter 0 - already freed!
          (Nothing, Weak (RcBox (# strong, weak, box #)))
        _ ->
          -- Can be upgraded - increment strong counter and proceed
          let !rcBox = incrStrong (RcBox (# strong, weak, box #))
           in (Just (Rc rcBox), Weak rcBox)
