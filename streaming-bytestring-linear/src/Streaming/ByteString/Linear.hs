{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- HLINT ignore "Avoid lambda" -}

module Streaming.ByteString.Linear (
  ByteStream (),
  empty,
  singleton,
  pack,
  unpack,
  fromChunks,
  toChunks,
  fromStrict,
  toStrict_,
  toStrict,
  fromLazy,
  toLazy_,
  toLazy,
  null,
  null_,
  testNull,
  denull,
  length_,
  length,
  cons,
  cons',
  snoc,
  head,
  uncons,
  unconsChunk,
  last,
  append,
  map,
  for,
  intersperse,
  foldr,
  fold,
  fold_,
) where

import Control.Functor.Linear
import qualified Control.Functor.Linear as Control
import Control.Monad.IO.Class.Linear
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy.Internal as LB
import qualified Data.ByteString.Unsafe as B
import qualified Data.Functor.Linear as Data
import Data.Word (Word8)
import Foreign (Storable (..))
import Foreign.Ptr
import GHC.Base (SPEC (..))
import GHC.ForeignPtr
import GHC.IO.Unsafe (unsafePerformIO)
import Prelude.Linear hiding (foldr, head, intersperse, last, length, map, null, uncons)
import Streaming.ByteString.Linear.Internal
import Streaming.Linear (destroy)
import Streaming.Prelude.Linear (Of (..), Stream (..))
import qualified Streaming.Prelude.Linear as SP
import qualified System.IO.Linear as LIO
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

{- | /O(1)/ The empty 'ByteStream' -- i.e. @return ()@ Note that @ByteStream m w@ is
generally a monoid for monoidal values of @w@, like @()@.
-}
empty :: ByteStream m ()
empty = Empty ()
{-# INLINE empty #-}

-- | /O(1)/ Yield a 'Word8' as a minimal 'ByteStream'.
singleton :: Word8 -> ByteStream m ()
singleton w = Chunk (B.singleton w) (Empty ())
{-# INLINE singleton #-}

-- | /O(n)/ Convert a monadic stream of individual 'Word8's into a packed byte stream.
pack :: (Monad m) => Stream (Of Word8) m r %1 -> ByteStream m r
pack = packBytes
{-# INLINE pack #-}

-- | /O(n)/ Converts a packed byte stream into a stream of individual bytes.
unpack :: (Monad m) => ByteStream m r %1 -> Stream (Of Word8) m r
unpack = unpackBytes

{- | /O(c)/ Convert a monadic stream of individual strict 'ByteString' chunks
into a byte stream.
-}
fromChunks :: (Monad m) => Stream (Of B.ByteString) m r %1 -> ByteStream m r
fromChunks cs = destroy cs (\(bs :> rest) -> Chunk bs rest) Go pure
{-# INLINE fromChunks #-}

{- | /O(c)/ Convert a byte stream into a stream of individual strict
bytestrings. This of course exposes the internal chunk structure.
-}
toChunks :: (Monad m) => ByteStream m r %1 -> Stream (Of B.ByteString) m r
toChunks bs = dematerialize bs pure (\b mx -> Step (b :> mx)) Effect
{-# INLINE toChunks #-}

-- | /O(1)/ Yield a strict 'ByteString' chunk.
fromStrict :: B.ByteString -> ByteStream m ()
fromStrict bs
  | B.null bs = Empty ()
  | otherwise = Chunk bs (Empty ())
{-# INLINE fromStrict #-}

{- | /O(n)/ Convert a byte stream into a single strict 'ByteString'.

Note that this is an /expensive/ operation that forces the whole monadic
ByteString into memory and then copies all the data. If possible, try to
avoid converting back and forth between streaming and strict bytestrings.
-}
toStrict_ :: (Monad m) => ByteStream m () -> m B.ByteString
toStrict_ = fmap (Unsafe.toLinear B.concat) . SP.toList_ . toChunks

{- | /O(n)/ Convert a monadic byte stream into a single strict 'ByteString',
retaining the return value of the original pair. This operation is for use
with 'mapped'.

> mapped R.toStrict :: Monad m => Stream (ByteStream m) m r -> Stream (Of ByteString) m r

It is subject to all the objections one makes to Data.ByteString.Lazy
'toStrict'; all of these are devastating.
-}
toStrict :: (Monad m) => ByteStream m r -> m (Of B.ByteString r)
toStrict bs = Control.do
  (bss :> r) <- SP.toList (toChunks bs)
  pure (B.concat bss :> r)
{-# INLINE toStrict #-}

{- | /O(c)/ Transmute a pseudo-pure lazy bytestring to its representation as a
 monadic stream of chunks.

 >>> Q.putStrLn $ Q.fromLazy "hi"
 hi
 >>>  Q.fromLazy "hi"
 Chunk "hi" (Empty (()))  -- note: a 'show' instance works in the identity monad
 >>>  Q.fromLazy $ BL.fromChunks ["here", "are", "some", "chunks"]
 Chunk "here" (Chunk "are" (Chunk "some" (Chunk "chunks" (Empty (())))))
-}
fromLazy :: LB.ByteString -> ByteStream m ()
fromLazy = LB.foldrChunks Chunk (Empty ())
{-# INLINE fromLazy #-}

{- | /O(n)/ Convert an effectful byte stream into a single lazy 'ByteStream'
with the same internal chunk structure. See `toLazy` which preserve
connectedness by keeping the return value of the effectful bytestring.
-}
toLazy_ :: (Monad m, Consumable r) => ByteStream m r %1 -> m LB.ByteString
toLazy_ bs =
  dematerialize
    bs
    (`lseq` pure LB.Empty)
    (fmap . LB.Chunk)
    join
{-# INLINE toLazy_ #-}

{- | /O(n)/ Convert an effectful byte stream into a single lazy 'ByteString'
with the same internal chunk structure, retaining the original return value.

This is the canonical way of breaking streaming (`toStrict` and the like are
far more demonic). Essentially one is dividing the interleaved layers of
effects and bytes into one immense layer of effects, followed by the memory
of the succession of bytes.

Because one preserves the return value, `toLazy` is a suitable argument for
'Streaming.mapped':

> S.mapped Q.toLazy :: Stream (ByteStream m) m r -> Stream (Of L.ByteString) m r

>>> Q.toLazy "hello"
"hello" :> ()
>>> S.toListM $ traverses Q.toLazy $ Q.lines "one\ntwo\nthree\nfour\nfive\n"
["one","two","three","four","five",""]  -- [L.ByteString]
-}
toLazy :: (Monad m) => ByteStream m r %1 -> m (Of LB.ByteString r)
toLazy bs0 =
  dematerialize
    bs0
    (\r -> pure (LB.Empty :> r))
    ( \b mx -> Control.do
        (bs :> x) <- mx
        pure $ LB.Chunk b bs :> x
    )
    join
{-# INLINE toLazy #-}

{- | Test whether a `ByteStream` is empty, collecting its return value; to reach
the return value, this operation must check the whole length of the string.

>>> Q.null "one\ntwo\three\nfour\nfive\n"
False :> ()
>>> Q.null ""
True :> ()
>>> S.print $ mapped R.null $ Q.lines "yours,\nMeredith"
False
False

Suitable for use with `SP.mapped`:

@
S.mapped Q.null :: Streaming (ByteStream m) m r -> Stream (Of Bool) m r
@
-}
null :: (Monad m) => ByteStream m r %1 -> m (Of Bool r)
null (Empty r) = pure (True :> r)
null (Go m) = m >>= null
null (Chunk bs rest) =
  if B.null bs
    then null rest
    else Control.do
      r <- SP.effects (toChunks rest)
      pure (False :> r)
{-# INLINEABLE null #-}

{- | /O(c)/ Test whether a `ByteStream` is empty. The value is of course in the
monad of the effects.

>>>  Q.null "one\ntwo\three\nfour\nfive\n"
False
>>> Q.null $ Q.take 0 Q.stdin
True
>>> :t Q.null $ Q.take 0 Q.stdin
Q.null $ Q.take 0 Q.stdin :: MonadIO m => m Bool
-}
null_ :: (Monad m, Consumable r) => ByteStream m r %1 -> m Bool
null_ (Empty x) = x `lseq` pure True
null_ (Go m) = m >>= null_
null_ (Chunk bs rest) =
  if B.null bs
    then null_ rest
    else False <$ consumeM rest
{-# INLINEABLE null_ #-}

consumeM :: (Monad m, Consumable r) => ByteStream m r %1 -> m ()
consumeM (Empty x) = pure (consume x)
consumeM (Go m) = m >>= consumeM
consumeM (Chunk _ rest) = consumeM rest
{-# INLINEABLE consumeM #-}

{- | Similar to `null`, but yields the remainder of the `ByteStream` stream when
an answer has been determined.
-}
testNull :: (Monad m) => ByteStream m r %1 -> m (Of Bool (ByteStream m r))
testNull (Empty r) = pure (True :> Empty r)
testNull (Go m) = m >>= testNull
testNull (Chunk bs rest) =
  if B.null bs
    then testNull rest
    else pure (False :> Chunk bs rest)
{-# INLINEABLE testNull #-}

-- | Remove empty ByteStrings from a stream of bytestrings.
denull ::
  forall m r.
  (Monad m) =>
  Stream (ByteStream m) m r %1 ->
  Stream (ByteStream m) m r
{-# INLINEABLE denull #-}
denull = loop . Right
  where
    -- Scan each substream, dropping empty chunks along the way.  As soon as a
    -- non-empty chunk is found, just apply the loop to the next substream in
    -- the terminal value via fmap.  If Empty comes up before that happens,
    -- continue the current stream instead with its denulled tail.
    --
    -- This implementation is tail recursive:
    -- \* Recursion via 'loop . Left' continues scanning an inner ByteStream.
    -- \* Recursion via 'loop . Right' moves to the next substream.
    --
    -- The old version below was shorter, but noticeably slower, especially
    -- when empty substreams are frequent:
    --
    --    denull = hoist (run . maps effects) . separate . mapped nulls
    --
    loop ::
      Either
        (ByteStream m (Stream (ByteStream m) m r))
        (Stream (ByteStream m) m r) %1 ->
      Stream (ByteStream m) m r
    loop = \case
      Left mbs ->
        mbs & \case
          Chunk c cs
            | B.length c > 0 -> Step $ Chunk c $ fmap (loop . Right) cs
            | otherwise -> loop $ Left cs
          Go m -> Effect $ loop . Left <$> m
          Empty r -> loop $ Right r
      Right strm ->
        strm & \case
          Step mbs ->
            mbs & \case
              Chunk c cs
                | B.length c > 0 -> Step $ Chunk c $ fmap (loop . Right) cs
                | otherwise -> loop $ Left cs
              Go m -> Effect $ loop . Left <$> m
              Empty r -> loop $ Right r
          Effect m -> Effect $ fmap (loop . Right) m
          Return r -> Return r

{- | Like `length`, report the length in bytes of the `ByteStream` by running
through its contents. Since the return value is in the effect @m@, this is
one way to "get out" of the stream.
-}
length_ :: (Monad m, Consumable r) => ByteStream m r %1 -> m Int
length_ =
  fmap (\(i :> r) -> r `lseq` i)
    . chunkFold (\n c -> n + B.length c) 0 id
{-# INLINE length_ #-}

{- | /O(n\/c)/ 'length' returns the length of a byte stream as an 'Int' together
with the return value. This makes various maps possible.

>>> Q.length "one\ntwo\three\nfour\nfive\n"
23 :> ()
>>> S.print $ S.take 3 $ mapped Q.length $ Q.lines "one\ntwo\three\nfour\nfive\n"
3
8
4
-}
length :: (Monad m) => ByteStream m r %1 -> m (Of Int r)
length = chunkFold (\n c -> n + B.length c) 0 id
{-# INLINE length #-}

-- | /O(1)/ 'cons' is analogous to @(:)@ for lists.
cons :: Word8 -> ByteStream m r %1 -> ByteStream m r
cons = Chunk P.. B.singleton
{-# INLINE cons #-}

{- | /O(1)/ Unlike 'cons', 'cons\'' is strict in the ByteString that we are
consing onto. More precisely, it forces the head and the first chunk. It does
this because, for space efficiency, it may coalesce the new byte onto the
first \'chunk\' rather than starting a new \'chunk\'.

So that means you can't use a lazy recursive contruction like this:

> let xs = cons\' c xs in xs

You can however use 'cons', as well as 'repeat' and 'cycle', to build
infinite byte streams.
-}
cons' :: Word8 -> ByteStream m r -> ByteStream m r
cons' w (Chunk c cs) | B.length c < 16 = Chunk (B.cons w c) cs
cons' w cs = Chunk (B.singleton w) cs
{-# INLINE cons' #-}

-- | /O(n\/c)/ Append a byte to the end of a 'ByteStream'.
snoc :: (Monad m) => ByteStream m r -> Word8 -> ByteStream m r
snoc cs w = Control.do
  r <- cs
  singleton w
  pure r
{-# INLINE snoc #-}

{- | /O(c)/ Extract the first element of a 'ByteStream', if there is one.
Suitable for use with `SP.mapped`:

@
S.mapped Q.head :: Stream (Q.ByteStream m) m r -> Stream (Of (Maybe Word8)) m r
@
-}
head :: (Monad m) => ByteStream m r %1 -> m (Of (Maybe Word8) r)
head (Empty r) = pure (Nothing :> r)
head (Chunk c rest) = case B.uncons c of
  Nothing -> head rest
  Just (w, _) -> Control.do
    r <- SP.effects $ toChunks rest
    pure $! Just w :> r
head (Go m) = m >>= head
{-# INLINEABLE head #-}

{- | /O(1)/ Extract the head and tail of a 'ByteStream', or its return value if
it is empty. This is the \'natural\' uncons for an effectful byte stream.
-}
uncons :: (Monad m) => ByteStream m r %1 -> m (Either r (Word8, ByteStream m r))
uncons (Chunk c@(B.length -> len) cs)
  | len > 0 =
      let !h = B.unsafeHead c
       in pure $
            Right
              ( h
              , if len > 1 then Chunk (B.unsafeTail c) cs else cs
              )
  | otherwise = uncons cs
uncons (Go m) = m >>= uncons
uncons (Empty r) = pure (Left r)
{-# INLINEABLE uncons #-}

{- | Like `uncons`, but yields the entire first `B.ByteString` chunk that the
stream is holding onto. If there wasn't one, it tries to fetch it.  Yields
the final @r@ return value when the 'ByteStream' is empty.
-}
unconsChunk :: (Monad m) => ByteStream m r %1 -> m (Either r (B.ByteString, ByteStream m r))
unconsChunk (Chunk c cs)
  | B.null c = unconsChunk cs
  | otherwise = pure (Right (c, cs))
unconsChunk (Go m) = m >>= unconsChunk
unconsChunk (Empty r) = pure (Left r)
{-# INLINEABLE unconsChunk #-}

{- | Extract the last element of a `ByteStream`, if possible. Suitable for use
with `SP.mapped`:

@
S.mapped Q.last :: Streaming (ByteStream m) m r -> Stream (Of (Maybe Word8)) m r
@
-}
last :: (Monad m) => ByteStream m r %1 -> m (Of (Maybe Word8) r)
last (Empty r) = pure (Nothing :> r)
last (Go m) = m >>= last
last (Chunk c0 cs0) = go c0 cs0
  where
    go c (Empty r) = pure (Just (B.unsafeLast c) :> r)
    go _ (Chunk c cs) = go c cs
    go x (Go m) = m >>= go x
{-# INLINEABLE last #-}

-- | /O(n\/c)/ Append two `ByteString`s together.
append ::
  (Consumable r, Monad m) =>
  ByteStream m r %1 ->
  ByteStream m s %1 ->
  ByteStream m s
append xs ys = dematerialize xs (`lseq` ys) Chunk Go
{-# INLINE append #-}

{- | /O(n)/ 'map' @f xs@ is the ByteStream obtained by applying @f@ to each
element of @xs@.
-}
map :: (Monad m) => (Word8 -> Word8) -> ByteStream m r %1 -> ByteStream m r
map f z = dematerialize z Empty (Chunk P.. B.map f) Go
{-# INLINE map #-}

{- | @'for' xs f@ applies @f@ to each chunk in the stream, and
concatenates the resulting streams.

Generalised in 0.2.4 to match @streaming@: the callback's (ignored)
return value can be of any type.

@since 0.2.3
-}
for ::
  (Monad m, Consumable x) =>
  ByteStream m r %1 ->
  (B.ByteString -> ByteStream m x) ->
  ByteStream m r
for stream f =
  stream & \case
    Empty r -> Empty r
    Chunk bs bss -> (consume <$> f bs) >> for bss f
    Go m -> Go ((`for` f) <$> m)
{-# INLINE for #-}

-- -- | /O(n)/ 'reverse' @xs@ returns the elements of @xs@ in reverse order.
-- reverse :: ByteString -> ByteString
-- reverse cs0 = rev Empty cs0
--   where rev a Empty        = a
--         rev a (Chunk c cs) = rev (Chunk (B.reverse c) a) cs
-- {-# INLINE reverse #-}

{- | The 'intersperse' function takes a 'Word8' and a 'ByteStream' and
\`intersperses\' that byte between the elements of the 'ByteStream'. It is
analogous to the intersperse function on Streams.
-}
intersperse :: (Monad m) => Word8 -> ByteStream m r %1 -> ByteStream m r
intersperse _ (Empty r) = Empty r
intersperse w (Go m) = Go (fmap (intersperse w) m)
intersperse w (Chunk c cs)
  | B.null c = intersperse w cs
  | otherwise =
      Chunk
        (B.intersperse w c)
        (dematerialize cs Empty (Chunk P.. intersperse') Go)
  where
    intersperse' :: B.ByteString -> B.ByteString
    intersperse' (B.PS fp o l)
      | l > 0 = B.unsafeCreate (2 * l) $ \p' -> unsafeWithForeignPtr fp $ \p -> do
          poke p' w
          B.c_intersperse (p' `plusPtr` 1) (p `plusPtr` o) (fromIntegral l) w
      | otherwise = B.empty
{-# INLINEABLE intersperse #-}

foldr :: (Monad m) => (Word8 -> a %1 -> a) -> a -> ByteStream m () %1 -> m a
foldr k = foldrChunks \chk a ->
  Unsafe.toLinear (B.foldr (\w8 x -> k w8 x)) a chk
{-# INLINE foldr #-}

{- | 'fold' keeps the return value of the left-folded bytestring. Useful for
simultaneous folds over a segmented bytestream.
-}
fold ::
  forall m x b r.
  (Monad m) =>
  (x -> Word8 -> x) ->
  x ->
  (x -> b) ->
  ByteStream m r %1 ->
  m (Of b r)
fold step0 begin finish p0 = loop p0 begin
  where
    loop :: ByteStream m r %1 -> x -> m (Of b r)
    loop (Chunk bs bss) !x = loop bss $! B.foldl' step0 x bs
    loop (Go m) !x = m >>= \p -> loop p x
    loop (Empty r) !x = pure (finish x :> r)
{-# INLINEABLE fold #-}

{- | 'fold_', applied to a binary operator, a starting value (typically the
left-identity of the operator), and a ByteStream, reduces the ByteStream
using the binary operator, from left to right. We use the style of the foldl
library for left folds
-}
fold_ ::
  forall m x b.
  (Monad m) =>
  (x -> Word8 -> x) ->
  x ->
  (x -> b) ->
  ByteStream m () ->
  m b
fold_ step0 begin finish p0 = loop p0 begin
  where
    loop :: ByteStream m () %1 -> x -> m b
    loop (Chunk bs bss) !x = loop bss $! B.foldl' step0 x bs
    loop (Go m) !x = m >>= \p -> loop p x
    loop (Empty ()) !x = pure (finish x)
{-# INLINEABLE fold_ #-}
