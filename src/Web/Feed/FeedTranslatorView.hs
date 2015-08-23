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
    select ! A.name name' ! A.id name' $ do
        option ! A.disabled "disabled"
               ! A.selected "selected"
               ! A.value ""
               $ "Choose language"
        option ! A.value "ko" $ "한국어 (ko)"
        option ! A.value "ja" $ "日本語 (ja)"

materializeCss :: AttributeValue
materializeCss = "//cdnjs.cloudflare.com/ajax/libs/materialize/0.97.0/css/materialize.min.css"

materializeJs :: AttributeValue
materializeJs = "//cdnjs.cloudflare.com/ajax/libs/materialize/0.97.0/js/materialize.min.js"

jQueryJs :: AttributeValue
jQueryJs = "//code.jquery.com/jquery-2.1.4.min.js"

index :: Html
index =
    docTypeHtml $ do
        H.head $ do
            meta ! A.charset "utf8"
            title "Feed Translator"
            link ! A.rel "stylesheet" ! A.href materializeCss
        body $ do
            nav $
                H.div ! A.class_ "container" $
                    H.div ! A.class_ "nav-wrapper" $
                        a ! A.class_ "brand-logo"
                          ! A.href "/"
                          $ "Feed Translator"
            H.div ! A.class_ "container" $ do
                p $ do
                    "Powered by "
                    a ! A.href "http://translate.naver.com/"
                      $ "Naver Translate"
                    "."
                form ! A.action "/query/" $ do
                    H.div ! A.class_ "row" $ do
                        H.div ! A.class_ "input-field col s6" $ do
                            languageSelector "source"
                            label ! A.for "source" $ "From"
                        H.div ! A.class_ "input-field col s6" $ do
                            languageSelector "target"
                            label ! A.for "target" $ "To"
                    H.div ! A.class_ "row" $
                        H.div ! A.class_ "input-field col s12" $ do
                            input ! A.id "url"
                                  ! A.name "url"
                                  ! A.type_ "url"
                                  ! A.class_ "validate"
                            label ! A.for "url" $ "URL"
                    H.div ! A.class_ "row" $
                        H.div ! A.class_ "input-field col s12" $
                            button ! A.type_ "submit"
                                   ! A.class_ "waves-effect waves-light btn"
                                   $ "Translate"
            script ! A.src jQueryJs $ ""
            script ! A.src materializeJs $ ""
            script "jQuery('select').material_select();"
