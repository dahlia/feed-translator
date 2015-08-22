{-# LANGUAGE OverloadedStrings #-}
module Web.Feed.FeedTranslatorWai (application) where

import Control.Lens.Getter ((^.))
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Data.LanguageCodes (ISO639_1, fromChars)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.Encoding as LTE
import Language.Translate.Naver (translate, translateUrl)
import Network.Wai (Application, rawQueryString)
import qualified Network.Wreq as W
import Text.Feed.Types (Feed)
import Text.Feed.Export (xmlFeed)
import Text.Feed.Import (parseFeedSource)
import Text.XML.Light.Output (showElement)
import Web.Scotty (ActionM, get, html, next, param, raw, regex, request,
                   scottyApp, setHeader)

import Web.Feed.FeedTranslator (Translator(..), translateFeed)

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
            let xmlString = response ^. W.responseBody
                feed = parseFeedSource $ LTE.decodeUtf8 xmlString
            case feed of
                Just feed' -> do
                    translatedFeed <- translateFeed translator feed'
                    return translatedFeed
                Nothing ->
                    ioError $ userError $ "failed to parse feed"
        _ -> ioError $ userError $ "bad request: " ++ url

parseLanguageCode :: ActionM LT.Text -> ActionM ISO639_1
parseLanguageCode code = do
    code' <- code
    case LT.unpack code' of
        [a, b] -> case fromChars a b of
            Just lang -> return lang
            Nothing -> next
        _ -> next

-- | WAI web application.
application :: IO Application
application = scottyApp $ do
    get (regex "^/([a-z]{2})/([a-z]{2})/(http|https)/(.*)") $ do
        source <- parseLanguageCode $ param "1"
        target <- parseLanguageCode $ param "2"
        scheme <- liftM LT.toStrict $ param "3"
        path <- liftM LT.toStrict $ param "4"
        query <- liftM (decodeUtf8 . rawQueryString) request
        let url :: String
            url = T.unpack $ T.concat [scheme, "://", path, query]
        let translator = naverTranslator source target
        feed <- liftIO $ translateFeedUrl translator url
        let feedXml = LT.pack $ showElement $ xmlFeed feed
        setHeader "Content-Type" "text/xml; charset=utf8"
        raw $ LTE.encodeUtf8 feedXml
