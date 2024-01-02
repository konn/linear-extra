{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Foreign.Atomic.Internal (
  synchronise,
  synchronise#,
  fetchAddWordOffset,
  fetchSubWordOffset,
  addFetchWordOffset,
  subFetchWordOffset,
) where

import GHC.Exts
import GHC.IO (unIO)

synchronise# :: State# RealWorld -> State# RealWorld
synchronise# s = case unIO synchronise s of
  (# !s', () #) -> s'

foreign import ccall unsafe "atomic.c sync_synchronize"
  synchronise :: IO ()

-- | Atomically add a value to a memory location and returns /old/ value.
foreign import ccall unsafe "atomic.c sync_fetch_add"
  fetchAddWordOffset :: Ptr Word -> Int -> Word -> IO Word

-- | Atomically subtract a value from a memory location and returns /old/ value.
foreign import ccall unsafe "atomic.c sync_fetch_sub"
  fetchSubWordOffset :: Ptr Word -> Int -> Word -> IO Word

-- | Atomically add a value to a memory location and returns /new/ value.
foreign import ccall unsafe "atomic.c sync_add_fetch"
  addFetchWordOffset :: Ptr Word -> Int -> Word -> IO Word

-- | Atomically subtract a value to a memory location and returns /new/ value.
foreign import ccall unsafe "atomic.c sync_sub_fetch"
  subFetchWordOffset :: Ptr Word -> Int -> Word -> IO Word
