{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Foreign.Atomic.Internal (
  synchronise,
  synchronise#,
  fetchAddWordOffset,
  fetchSubWordOffset,
) where

import GHC.Exts
import GHC.IO (unIO)

synchronise# :: State# RealWorld -> State# RealWorld
synchronise# s = case unIO synchronise s of
  (# !s', () #) -> s'

foreign import ccall unsafe "atomic.c sync_synchronize"
  synchronise :: IO ()

foreign import ccall unsafe "atomic.c sync_fetch_add"
  fetchAddWordOffset :: Ptr Word -> Int -> Word -> IO Word

foreign import ccall unsafe "atomic.c sync_fetch_sub"
  fetchSubWordOffset :: Ptr Word -> Int -> Word -> IO Word
