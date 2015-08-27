{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Web.Feed.FeedTranslatorView (index) where

import Control.Monad (forM_)
import Data.List (intercalate)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Blaze (dataAttribute)
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html5
import Text.Blaze.Internal (AttributeValue, stringValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.RawString.QQ

languagePairs :: M.Map String (S.Set String)
languagePairs = M.fromList [ ("en", S.fromList ["ko", "es", "pt", "id", "th"])
                           , ("ko", S.fromList ["en", "ja"])
                           , ("ja", S.fromList ["ko"])
                           , ("es", S.fromList ["en"])
                           , ("pt", S.fromList ["en"])
                           , ("id", S.fromList ["en"])
                           , ("th", S.fromList ["en"])
                           ]

languageNames :: M.Map String String
languageNames = M.fromList [ ("en", "English")
                           , ("ko", "한국어")
                           , ("ja", "日本語")
                           , ("es", "español")
                           , ("pt", "português")
                           , ("id", "Bahasa Indonesia")
                           , ("th", "ไทย")
                           ]

languageSelector :: AttributeValue -- | The 'name' of 'select' element.
                 -> Html
languageSelector name' =
    select ! A.name name' ! A.id name' ! A.class_ "browser-default" $ do
        let langs = M.toList languageNames
        forM_ langs $ \(code, name') ->
            let targets = case M.lookup code languagePairs of
                              Just set -> intercalate "," $ S.toList set
                              Nothing -> ""
                opt = option ! A.value (stringValue code)
                             ! dataAttribute "targets" (stringValue targets)
            in case targets of
                   "" -> opt ! A.disabled "disabled"
                   _ -> opt
               $ toHtml (name' ++ " [" ++ code ++ "]")

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
                        H.div ! A.class_ "col s6" $ do
                            H.div ! A.class_ "row" $
                                H.div ! A.class_ "input-field col s12" $
                                    label ! A.for "source" $ "From"
                            H.div ! A.class_ "row" $
                                H.div ! A.class_ "input-field col s12" $
                                    languageSelector "source"
                        H.div ! A.class_ "col s6" $ do
                            H.div ! A.class_ "row" $
                                H.div ! A.class_ "input-field col s12" $
                                    label ! A.for "target" $ "To"
                            H.div ! A.class_ "row" $
                                H.div ! A.class_ "input-field col s12" $
                                    languageSelector "target"
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
            script [r|
                (function ($) {
                    var filterTarget = function () {
                        var sourceOption = $('option:selected', this);
                        var dataTargets = sourceOption.data('targets') || '';
                        var targets = dataTargets.split(',');
                        $('select[name=target] > option').prop(
                            'disabled',
                            function () {
                                return targets.indexOf(this.value) < 0;
                            }
                        );
                        $('select[name=target] > ' +
                          'option:not(:disabled):first').prop('selected', true);
                    };
                    var source = $('select[name=source]').change(filterTarget)
                                                         .each(filterTarget);
                })(jQuery);
            |]
