module Web.Feed.FeedTranslatorCache (CacheMap, wrapTranslate) where

import Data.IORef (IORef, atomicModifyIORef', readIORef, writeIORef)

import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Web.Feed.FeedTranslator (TextTranslator)

-- | Map type to store cache.
type CacheMap = M.Map ByteString Text

-- | Translate the text with the given translator engine, and make it cached.
wrapTranslate :: IORef CacheMap -- | The reference to a cache store
              -> TextTranslator -- | The actual translator engine to wrap
              -> TextTranslator
wrapTranslate ref translate text = do
    let digest = SHA1.hash $ encodeUtf8 text
    store <- readIORef ref
    let cache = M.lookup digest store
    case cache of
        Just cached -> return cached
        Nothing -> do
            translated <- translate text
            atomicModifyIORef' ref $ \store' ->
                (M.insert digest translated store', translated)
