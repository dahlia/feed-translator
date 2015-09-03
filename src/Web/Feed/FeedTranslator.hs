module Web.Feed.FeedTranslator ( TextTranslator
                               , Translator(..)
                               , UrlTranslator
                               , translateFeed
                               ) where

import Control.Monad (liftM)
import Data.Text (Text, pack, unpack)
import Network.URI (URI, parseAbsoluteURI)
import Text.Feed.Constructor (withFeedItems, withFeedTitle, withItemDescription,
                              withItemId, withItemLink, withItemTitle)
import Text.Feed.Types (Feed, Item)
import Text.Feed.Query (feedItems, getFeedTitle, getItemId, getItemDescription,
                        getItemLink, getItemSummary, getItemTitle)

-- Text translator engine type.
type TextTranslator = Text -> IO Text

-- Link translator engine type.
type UrlTranslator = URI -> Maybe URI

-- Translator engine type combining 'TextTranslator' and 'UrlTranslator'.
data Translator = Translator { textTranslator :: TextTranslator
                             , urlTranslator :: UrlTranslator
                             }

-- | Translate the given feed.
translateFeed :: Translator -> Feed -> IO Feed
translateFeed translator feed = do
    let title :: Text
        title = pack $ getFeedTitle feed
        items :: [Item]
        items = feedItems feed
    translatedTitle <- textTranslator translator title
    translatedItems <- mapM (translateItem translator) items
    return $ withFeedItems translatedItems
           $ withFeedTitle (unpack translatedTitle) feed

-- | Translate a single item.
translateItem :: Translator -> Item -> IO Item
translateItem (Translator translate translateUrl) item = do
    let itemId :: Maybe (Bool, String)
        itemId = getItemId item
        desc :: Maybe String
        desc = case getItemDescription item of
                    Nothing -> getItemSummary item
                    Just "" -> getItemSummary item
                    Just s -> Just s
        link :: Maybe String
        link = getItemLink item
        title :: Maybe String
        title = getItemTitle item
        tId :: Maybe String
        tId = case itemId of
                  Just (True, id') -> Just $ translateLink id'
                  Just (False, id') -> Just id'
                  Nothing -> Nothing
        uri :: Bool
        uri = case itemId of
                  Just (True, _) -> True
                  _ -> False
        tLink :: Maybe String
        tLink = liftM translateLink link
    tTitle <- translate' title
    tDesc <- translate' desc
    let replace :: Item -> Item
        replace = setter tId (withItemId uri) .
                  setter tLink withItemLink .
                  setter tTitle withItemTitle .
                  setter tDesc withItemDescription
    return $ replace item
  where
    translateLink :: String -> String
    translateLink urlString =
        case parseAbsoluteURI urlString of
            Just uri -> case translateUrl uri of
                Just translatedUri -> show translatedUri
                Nothing -> urlString
            Nothing -> urlString
    translate' :: Maybe String -> IO (Maybe String)
    translate' s =
        case s of
            Just s -> liftM (Just . unpack) $ translate $ pack s
            Nothing -> return Nothing
    setter :: Maybe a -> (a -> Item -> Item) -> Item -> Item
    setter value set =
        case value of
            Just value -> set value
            Nothing -> id
