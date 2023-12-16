{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Streaming.ByteString.Linear.Internal (
  ByteStream (..),
  hoist,
  consChunk,
  chunkOverhead,
  defaultChunkSize,
  smallChunkSize,
  materialize,
  dematerialize,
  foldrChunks,
  foldlChunks,
  mwrap,
  chunk,
  packBytes,
  packChars,
  unpackBytes,
  chunkMap,
  chunkMapM,
  chunkMapM_,
  chunkFold,
  chunkFoldM,
  unfoldMChunks,
  unfoldrChunks,
  reread,
  copy,
) where

import qualified Control.Functor.Linear as Control
import Control.Monad.IO.Class.Linear
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.Functor.Linear as Data
import Data.Word (Word8)
import Foreign (Storable (..))
import Foreign.Ptr
import GHC.Base (SPEC (..))
import GHC.ForeignPtr
import Prelude.Linear
import Streaming.Prelude.Linear (Of (..), Stream (..))
import qualified Streaming.Prelude.Linear as SP
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as P

data ByteStream m r where
  Empty :: r %1 -> ByteStream m r
  Chunk :: {-# UNPACK #-} !B.ByteString -> ByteStream m r %1 -> ByteStream m r
  Go :: m (ByteStream m r) %1 -> ByteStream m r

instance (r ~ ()) => IsString (ByteStream m r) where
  fromString = chunk P.. B.pack P.. P.map B.c2w
  {-# INLINE fromString #-}

fmap' ::
  (Data.Functor m) =>
  (a %1 -> b) ->
  ByteStream m a %1 ->
  ByteStream m b
fmap' f (Empty r) = Empty (f r)
fmap' f (Chunk bs r) = Chunk bs $ Data.fmap f r
fmap' f (Go ms) = Go (Data.fmap (Data.fmap f) ms)

instance (Data.Functor m) => Data.Functor (ByteStream m) where
  fmap = fmap'
  {-# INLINEABLE fmap #-}

instance (Control.Functor m) => Data.Applicative (ByteStream m) where
  pure = Empty
  {-# INLINE pure #-}

  (<*>) = app
  {-# INLINEABLE (<*>) #-}

app ::
  (Control.Functor m) =>
  ByteStream m (a %1 -> b) %1 ->
  ByteStream m a %1 ->
  ByteStream m b
app (Empty f) stream = Control.fmap f stream
app (Chunk bs r) stream = Chunk bs $ r Data.<*> stream
app (Go ms) stream = Go $ Control.fmap (Data.<*> stream) ms

instance (Control.Functor m) => Control.Functor (ByteStream m) where
  fmap = fmap''
  {-# INLINEABLE fmap #-}

fmap'' ::
  (Control.Functor m) =>
  (a %1 -> b) %1 ->
  ByteStream m a %1 ->
  ByteStream m b
fmap'' f (Empty r) = Empty (f r)
fmap'' f (Chunk bs r) = Chunk bs $ Control.fmap f r
fmap'' f (Go ms) = Go (Control.fmap (Control.fmap f) ms)

instance (Control.Functor m) => Control.Applicative (ByteStream m) where
  pure = Empty
  {-# INLINE pure #-}

  (<*>) = (Data.<*>)
  {-# INLINE (<*>) #-}

bind ::
  forall m a b.
  (Control.Functor m) =>
  ByteStream m a %1 ->
  (a %1 -> ByteStream m b) %1 ->
  ByteStream m b
{-# INLINEABLE bind #-}
bind = loop SPEC2
  where
    loop ::
      SPEC ->
      ByteStream m a %1 ->
      (a %1 -> ByteStream m b) %1 ->
      ByteStream m b
    loop _ (Empty a) f = f a
    loop _ (Chunk bs r) f = Chunk bs $ loop SPEC r f
    loop _ (Go ms) f = Go $ Control.fmap (\x -> loop SPEC x f) ms

instance (Control.Functor m) => Control.Monad (ByteStream m) where
  (>>=) = bind
  {-# INLINEABLE (>>=) #-}

instance Control.MonadTrans ByteStream where
  lift = Go . Control.fmap Control.pure
  {-# INLINE lift #-}

hoist ::
  forall m n r.
  (Control.Monad m) =>
  (forall a. m a %1 -> n a) ->
  ByteStream m r %1 ->
  ByteStream n r
hoist phi = \case
  Empty r -> Empty r
  Chunk bs' rest -> Chunk bs' (hoist phi rest)
  Go m -> Go (phi (Control.fmap (hoist phi) m))
{-# INLINEABLE hoist #-}

-- | Smart constructor for 'Chunk'.
consChunk :: B.ByteString -> ByteStream m r %1 -> ByteStream m r
consChunk c@(B.PS _ _ len) cs
  | len == 0 = cs
  | otherwise = Chunk c cs
{-# INLINE consChunk #-}

-- | Yield-style smart constructor for 'Chunk'.
chunk :: B.ByteString -> ByteStream m ()
chunk bs = consChunk bs (Empty ())
{-# INLINE chunk #-}

{- | Reconceive an effect that results in an effectful bytestring as an effectful bytestring.
    Compare Streaming.mwrap. The closest equivalent of

>>> Streaming.wrap :: f (Stream f m r) -> Stream f m r

    is here  @consChunk@. @mwrap@ is the smart constructor for the internal @Go@ constructor.
-}
mwrap :: m (ByteStream m r) %1 -> ByteStream m r
mwrap = Go
{-# INLINE mwrap #-}

-- | Construct a succession of chunks from its Church encoding (compare @GHC.Exts.build@)
materialize :: (forall x. (r %1 -> x) %1 -> (B.ByteString -> x %1 -> x) -> (m x %1 -> x) -> x) %1 -> ByteStream m r
materialize phi = phi Empty Chunk Go
{-# INLINE [0] materialize #-}

{- | Resolve a succession of chunks into its Church encoding; this is
not a safe operation; it is equivalent to exposing the constructors
-}
dematerialize ::
  forall m r.
  (Control.Monad m) =>
  ByteStream m r %1 ->
  (forall x. (r %1 -> x) %1 -> (B.ByteString -> x %1 -> x) -> (m x %1 -> x) -> x)
dematerialize x0 (nil :: r %1 -> x) cons mwrap' = loop SPEC nil x0
  where
    loop :: SPEC -> (r %1 -> x) %1 -> ByteStream m r %1 -> x
    loop !_ nil (Empty r) = nil r
    loop !_ nil (Chunk b bs) = cons b (loop SPEC nil bs)
    loop !_ nil (Go ms) = mwrap' (Control.fmap (loop SPEC nil) ms)
{-# INLINE [1] dematerialize #-}

{-# RULES
"dematerialize/materialize" forall (phi :: forall b. (r %1 -> b) %1 -> (B.ByteString -> b %1 -> b) -> (m b %1 -> b) -> b). dematerialize (materialize phi) = phi
  #-}

-- | The chunk size used for I\/O. Currently set to 32k, less the memory management overhead
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
  where
    k = 1024
{-# INLINE defaultChunkSize #-}

-- | The recommended chunk size. Currently set to 4k, less the memory management overhead
smallChunkSize :: Int
smallChunkSize = 4 * k - chunkOverhead
  where
    k = 1024
{-# INLINE smallChunkSize #-}

-- | The memory management overhead. Currently this is tuned for GHC only.
chunkOverhead :: Int
chunkOverhead = 2 * sizeOf (undefined :: Int)
{-# INLINE chunkOverhead #-}

{- HLint ignore "Avoid lambda" -}
foldrChunks ::
  (Control.Monad m, Consumable r) =>
  (B.ByteString -> a %1 -> a) ->
  a ->
  ByteStream m r %1 ->
  m a
foldrChunks step nil bs =
  dematerialize
    bs
    (\x -> x `lseq` Control.pure nil)
    (\c -> Control.fmap (step c))
    Control.join
{-# INLINE foldrChunks #-}

foldlChunks ::
  forall m r a.
  (Control.Monad m, Consumable r) =>
  (a %1 -> B.ByteString -> a) ->
  a ->
  ByteStream m r %1 ->
  m a
{- HLint ignore foldlChunks "Eta reduce" -}
foldlChunks step nil = go nil
  where
    go :: a %1 -> ByteStream m r %1 -> m a
    go !a (Empty r) = r `lseq` Control.pure a
    go !a (Chunk bs r) = go (step a bs) r
    go !a (Go ms) = ms Control.>>= go a
{-# INLINE foldlChunks #-}

-- | Convert a `Stream` of pure `Word8` into a chunked 'ByteStream'.
packBytes :: (Control.Monad m) => Stream (Of Word8) m r %1 -> ByteStream m r
packBytes cs0 = Control.do
  -- XXX: Why 32?  It seems like a rather small chunk size, wouldn't
  -- smallChunkSize make a better choice?
  (bytes :> rest) <- Control.lift $ SP.toList $ SP.splitAt 32 cs0
  case bytes of
    [] ->
      rest & \case
        Return r -> Empty r
        Step as -> packBytes (Step as) -- these two pattern matches
        Effect m -> Go $ Control.fmap packBytes m -- should be avoided.
    _ -> Chunk (B.packBytes bytes) (packBytes rest)
{-# INLINEABLE packBytes #-}

-- | Convert a `Stream` of pure `Word8` into a chunked 'Charstream'.
packChars :: (Control.Monad m) => Stream (Of Char) m r %1 -> ByteStream m r
packChars cs0 = Control.do
  -- XXX: Why 32?  It seems like a rather small chunk size, wouldn't
  -- smallChunkSize make a better choice?
  (chars :> rest) <- Control.lift $ SP.toList $ SP.splitAt 32 cs0
  case chars of
    [] ->
      rest & \case
        Return r -> Empty r
        Step as -> packChars (Step as) -- these two pattern matches
        Effect m -> Go $ Control.fmap packChars m -- should be avoided.
    _ -> Chunk (B.packChars chars) (packChars rest)
{-# INLINEABLE packChars #-}

{- | The reverse of `packChars`. Given a stream of bytes, produce a `Stream`
individual bytes.
-}
unpackBytes ::
  (Control.Monad m) =>
  ByteStream m r %1 ->
  Stream (Of Word8) m r
unpackBytes bss = dematerialize bss Return unpackAppendBytesLazy Effect
  where
    unpackAppendBytesLazy ::
      B.ByteString ->
      Stream (Of Word8) m r %1 ->
      Stream (Of Word8) m r
    unpackAppendBytesLazy b@(B.PS fp off len) xs
      | len <= 100 = unpackAppendBytesStrict b xs
      | otherwise =
          unpackAppendBytesStrict (B.PS fp off 100) $
            unpackAppendBytesLazy (B.PS fp (off + 100) (len - 100)) xs

    unpackAppendBytesStrict ::
      B.ByteString -> Stream (Of Word8) m r %1 -> Stream (Of Word8) m r
    unpackAppendBytesStrict (B.PS fp off len) = Unsafe.toLinear $ \xs ->
      Unsafe.toLinear B.accursedUnutterablePerformIO $
        unsafeWithForeignPtr fp $ \base ->
          loop (base `plusPtr` (off - 1)) (base `plusPtr` (off - 1 + len)) xs
      where
        loop ::
          Ptr Word8 ->
          Ptr Word8 ->
          Stream (Of Word8) m r %1 ->
          IO (Stream (Of Word8) m r)
        loop !sentinel !p
          | p P.== sentinel = Unsafe.toLinear P.return
          | otherwise = Unsafe.toLinear \acc -> do
              x <- peek p
              loop sentinel (p `plusPtr` (-1)) (Step (x :> acc))
{-# INLINEABLE unpackBytes #-}

{- | Instead of mapping over each `Word8` or `Char`, map over each strict
`B.ByteString` chunk in the stream.
-}
chunkMap ::
  (Control.Monad m) =>
  (B.ByteString -> B.ByteString) ->
  ByteStream m r %1 ->
  ByteStream m r
chunkMap f bs = dematerialize bs Control.return (Chunk P.. f) Go
{-# INLINE chunkMap #-}

-- | Like `chunkMap`, but map effectfully.
chunkMapM ::
  (Control.Monad m) =>
  (B.ByteString -> m (Ur B.ByteString)) ->
  ByteStream m r %1 ->
  ByteStream m r
chunkMapM f bs =
  dematerialize
    bs
    Control.return
    (\bs' bss -> Go (Control.fmap (\(Ur bs'') -> Chunk bs'' bss) (f bs')))
    Go
{-# INLINE chunkMapM #-}

-- | Like `chunkMap`, but map effectfully.
chunkMapM_ ::
  (Control.Monad m, Consumable x) =>
  (B.ByteString -> m x) ->
  ByteStream m r %1 ->
  m r
chunkMapM_ f bs =
  dematerialize
    bs
    Control.return
    (\bs' mr -> Data.fmap consume (f bs') Control.>> mr)
    Control.join
{-# INLINE chunkMapM_ #-}

chunkFold ::
  forall m x a r.
  (Control.Monad m) =>
  (x -> B.ByteString -> x) ->
  x ->
  (x -> a) ->
  ByteStream m r %1 ->
  m (Of a r)
chunkFold step begin done = go begin
  where
    go :: x -> ByteStream m r %1 -> m (Of a r)
    go !a (Empty r) = Control.return (done a :> r)
    go !a (Chunk c cs) = go (step a c) cs
    go !a (Go m) = m Control.>>= go a
{-# INLINEABLE chunkFold #-}

chunkFoldM ::
  forall m x a r.
  (Control.Monad m) =>
  (x %1 -> B.ByteString -> m x) ->
  m x ->
  (x %1 -> m (Ur a)) ->
  ByteStream m r %1 ->
  m (Of a r)
chunkFoldM step begin done bs = begin Control.>>= go bs
  where
    go :: ByteStream m r %1 -> x %1 -> m (Of a r)
    go (Empty r) !a = done a Control.<&> \(Ur a) -> a :> r
    go (Chunk c cs) !a = step a c Control.>>= go cs
    go (Go m) !a = m Control.>>= \str -> go str a
{-# INLINEABLE chunkFoldM #-}

{- | Given some continual monadic action that produces strict `B.ByteString`
chunks, produce a stream of bytes.
-}
unfoldMChunks ::
  forall s m.
  (Control.Monad m) =>
  (s %1 -> m (Maybe (Of B.ByteString s))) ->
  s %1 ->
  ByteStream m ()
unfoldMChunks step = loop
  where
    loop :: s %1 -> ByteStream m ()
    loop s = Go Control.do
      m <- step s
      m & \case
        Nothing -> Control.return (Empty ())
        Just (bs :> s') -> Control.return $ Chunk bs (loop s')
{-# INLINEABLE unfoldMChunks #-}

-- | Like `unfoldMChunks`, but feed through a final @r@ return value.
unfoldrChunks ::
  forall m s r.
  (Control.Monad m) =>
  (s %1 -> m (Either r (Of B.ByteString s))) ->
  s %1 ->
  ByteStream m r
unfoldrChunks step = loop
  where
    loop :: s %1 -> ByteStream m r
    loop !s = Go Control.do
      m <- step s
      m & \case
        Left r -> Control.return (Empty r)
        Right (bs :> s') -> Control.return $ Chunk bs (loop s')
{-# INLINEABLE unfoldrChunks #-}

{- | Stream chunks from something that contains @m (Maybe ByteString)@ until it
returns 'Nothing'. 'reread' is of particular use rendering @io-streams@ input
streams as byte streams in the present sense.

> import qualified Data.ByteString as B
> import qualified System.IO.Streams as S
> Q.reread S.read            :: S.InputStream B.ByteString -> Q.ByteStream IO ()
> Q.reread (liftIO . S.read) :: MonadIO m => S.InputStream B.ByteString -> Q.ByteStream m ()

The other direction here is

> S.unfoldM Q.unconsChunk    :: Q.ByteString IO r -> IO (S.InputStream B.ByteString)
-}
reread ::
  forall m s.
  (Control.Monad m) =>
  (s -> m (Maybe (Ur B.ByteString))) ->
  s ->
  ByteStream m ()
reread step s = loop
  where
    loop :: ByteStream m ()
    loop = Go Control.do
      m <- step s
      m & \case
        Nothing -> Control.return (Empty ())
        Just (Ur a) -> Control.return (Chunk a loop)
{-# INLINEABLE reread #-}

{- | Make the information in a bytestring available to more than one eliminating fold, e.g.

>>>  Q.count 'l' $ Q.count 'o' $ Q.copy $ "hello\nworld"
3 :> (2 :> ())

>>> Q.length $ Q.count 'l' $ Q.count 'o' $ Q.copy $ Q.copy "hello\nworld"
11 :> (3 :> (2 :> ()))

>>> runResourceT $ Q.writeFile "hello2.txt" $ Q.writeFile "hello1.txt" $ Q.copy $ "hello\nworld\n"
>>> :! cat hello2.txt
hello
world
>>> :! cat hello1.txt
hello
world

    This sort of manipulation could as well be acheived by combining folds - using
    @Control.Foldl@ for example. But any sort of manipulation can be involved in
    the fold.  Here are a couple of trivial complications involving splitting by lines:

>>> let doubleLines = Q.unlines . maps (<* Q.chunk "\n" ) . Q.lines
>>> let emphasize = Q.unlines . maps (<* Q.chunk "!" ) . Q.lines
>>> runResourceT $ Q.writeFile "hello2.txt" $ emphasize $ Q.writeFile "hello1.txt" $ doubleLines $ Q.copy $ "hello\nworld"
>>> :! cat hello2.txt
hello!
world!
>>> :! cat hello1.txt
hello
<BLANKLINE>
world
<BLANKLINE>

    As with the parallel operations in @Streaming.Prelude@, we have

> Q.effects . Q.copy       = id
> hoist Q.effects . Q.copy = id

   The duplication does not by itself involve the copying of bytestring chunks;
   it just makes two references to each chunk as it arises. This does, however
   double the number of constructors associated with each chunk.
-}
copy ::
  forall m r.
  (Control.Monad m) =>
  ByteStream m r %1 ->
  ByteStream (ByteStream m) r
copy = loop
  where
    loop :: ByteStream m r %1 -> ByteStream (ByteStream m) r
    loop (Empty r) = Empty r
    loop (Go m) = Go (Control.fmap loop (Control.lift m))
    loop (Chunk bs rest) = Chunk bs (Go (Chunk bs (Empty (loop rest))))
{-# INLINEABLE copy #-}

instance (MonadIO m) => MonadIO (ByteStream m) where
  liftIO io = Go $ Control.fmap Empty (liftIO io)
  {-# INLINE liftIO #-}

instance (Semigroup r, Control.Applicative m) => Semigroup (ByteStream m r) where
  (<>) = Control.liftA2 (<>)
  {-# INLINE (<>) #-}

instance (Monoid r, Control.Applicative m) => Monoid (ByteStream m r) where
  mempty = Empty mempty
  {-# INLINE mempty #-}
