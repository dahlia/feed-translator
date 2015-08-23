{-# LANGUAGE OverloadedStrings #-}
module Web.Feed.FeedTranslatorView (index) where

import Text.Blaze.Html (Html)
import Text.Blaze.Html5
import Text.Blaze.Internal (AttributeValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

languageSelector :: AttributeValue -- | The 'name' of 'select' element.
                 -> Html
languageSelector name' =
    select ! A.name name' $ do
        option ! A.value "ko" $ "한국어 (ko)"
        option ! A.value "ja" $ "日本語 (ja)"

index :: Html
index =
    docTypeHtml $ do
        H.head $ do
            meta ! A.charset "utf8"
            title "Feed Translator"
        body $ do
            h1 "Feed Translator"
            p $ do
                "Powered by "
                a ! A.href "http://translate.naver.com/" $ "Naver Translate"
                "."
            form ! A.action "/query/" $
                fieldset $ do
                    label $ do
                        H.span "From"
                        languageSelector "source"
                    label $ do
                        H.span "To"
                        languageSelector "target"
                    label $ do
                        H.span "URL"
                        input ! A.name "url" ! A.type_ "url"
                    button ! A.type_ "submit" $ "Translate"
