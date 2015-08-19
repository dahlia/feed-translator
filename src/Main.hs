import Data.LanguageCodes (ISO639_1(JA, KO))
import Language.Translate.Naver (translate, translateUrl)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr, writeFile)
import Text.Feed.Export (xmlFeed)
import Text.Feed.Import (parseFeedFromFile)
import Text.XML.Light.Output (showElement)

import Web.Feed.FeedTranslator (Translator(..), translateFeed)

naverTranslator :: ISO639_1   -- | The language translated from
                -> ISO639_1   -- | The language to translate to
                -> Translator
naverTranslator sourceLanguage targetLanguage =
    Translator { textTranslator = translate sourceLanguage targetLanguage
               , urlTranslator = translateUrl sourceLanguage targetLanguage
               }

main = do
    prog <- getProgName
    args <- getArgs
    case args of
        [inFile, outFile] -> do
            feed <- parseFeedFromFile inFile
            translatedFeed <- translateFeed (naverTranslator JA KO) feed
            writeFile outFile $ showElement $ xmlFeed translatedFeed
        _ -> do
            hPutStrLn stderr $ "usage: " ++ prog ++ " INPUT OUTPUT"
            exitWith $ ExitFailure 1
