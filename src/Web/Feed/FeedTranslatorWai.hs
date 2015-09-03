{-# LANGUAGE OverloadedStrings #-}
module Web.Feed.FeedTranslatorWai (application) where

import Data.Maybe (catMaybes, listToMaybe)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import Control.Lens.Getter ((^.))
import Data.ByteString (ByteString)
import Data.LanguageCodes (ISO639_1, fromChars)
import Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.Encoding as LTE
import Language.Translate.Naver (translate, translateUrl)
import Network.Wai (Application, rawQueryString)
import qualified Network.Wreq as W
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Feed.Types (Feed)
import Text.Feed.Export (xmlFeed)
import Text.Feed.Import (readAtom, readRSS1, readRSS2)
import Text.XML.Light.Input (parseXMLDoc)
import Text.XML.Light.Output (showElement)
import Web.Scotty (ActionM, get, html, next, param, params, raw, redirect,
                   regex, request, scottyApp, setHeader)

import Web.Feed.FeedTranslator (Translator(..), translateFeed)
import Web.Feed.FeedTranslatorCache (CacheMap, wrapTranslate)
import Web.Feed.FeedTranslatorView (index)

-- | Return the Naver Translate engine that translates text from the
-- 'sourceLanguage' to 'targetLanguage'.
naverTranslator :: ISO639_1   -- | The language translated from
                -> ISO639_1   -- | The language to translate to
                -> Translator
naverTranslator sourceLanguage targetLanguage =
    Translator { textTranslator = translate sourceLanguage targetLanguage
               , urlTranslator = translateUrl sourceLanguage targetLanguage
               }

-- | Translate the feed of given 'url' using 'translator'.
translateFeedUrl :: Translator -- | The translator engine to use
                 -> String     -- | The feed url to translate
                 -> IO Feed    -- | The translated feed
translateFeedUrl translator url = do
    response <- W.get url
    case response ^. W.responseStatus . W.statusCode of
        200 -> do
            let xmlString = LTE.decodeUtf8 $ response ^. W.responseBody
                xmlDoc = parseXMLDoc xmlString
                readers = [readAtom, readRSS2, readRSS1]
                possibleFeeds = catMaybes [read =<< xmlDoc | read <- readers]
                feed = listToMaybe possibleFeeds
            case feed of
                Just feed' -> translateFeed translator feed'
                Nothing ->
                    ioError $ userError "failed to parse feed"
        _ -> ioError $ userError $ "bad request: " ++ url

parseLanguageCode :: ActionM LT.Text -> ActionM ISO639_1
parseLanguageCode code = do
    code' <- code
    case LT.unpack code' of
        [a, b] -> case fromChars a b of
            Just lang -> return lang
            Nothing -> next
        _ -> next

type CacheKey = (ISO639_1, ISO639_1)

-- | WAI web application.
application :: IORef (M.Map CacheKey (IORef CacheMap)) -> IO Application
application cacheMapRef = scottyApp $ do
    get "/" $
        html $ renderHtml index
    get "/query/" $ do
        source <- param "source"
        target <- param "target"
        url <- param "url"
        let getUrl scheme path = LT.concat [ "/", source, "/", target, "/"
                                           , scheme , "/", LT.drop 3 path
                                           ]
        case LT.breakOn "://" url of
            ("http", path) -> redirect $ getUrl "http" path
            ("https", path) -> redirect $ getUrl "https" path
            a -> redirect "/"
    get (regex "^/([a-z]{2})/([a-z]{2})/(http|https)/(.*)") $ do
        source <- parseLanguageCode $ param "1"
        target <- parseLanguageCode $ param "2"
        scheme <- liftM LT.toStrict $ param "3"
        path <- liftM LT.toStrict $ param "4"
        query <- liftM (decodeUtf8 . rawQueryString) request
        let cacheKey :: CacheKey
            cacheKey = (source, target)
        cacheMap <- liftIO $ readIORef cacheMapRef
        cacheRef <- liftIO $ case M.lookup cacheKey cacheMap of
            Just ref -> return ref
            Nothing -> do
                ref <- newIORef M.empty
                atomicModifyIORef' cacheMapRef $ \cacheMap ->
                    (M.insert cacheKey ref cacheMap, ref)
        let url :: String
            url = T.unpack $ T.concat [scheme, "://", path, query]
            translator :: Translator
            translator = naverTranslator source target
            cachedTranslator :: Translator
            cachedTranslator = translator {
                textTranslator = wrapTranslate cacheRef
                                               (textTranslator translator)
            }
        feed <- liftIO $ translateFeedUrl cachedTranslator url
        let feedXml = LT.pack $ showElement $ xmlFeed feed
        setHeader "Content-Type" "text/xml; charset=utf8"
        raw $ LTE.encodeUtf8 feedXml
