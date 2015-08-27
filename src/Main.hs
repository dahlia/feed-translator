{-# LANGUAGE DeriveDataTypeable #-}
import Data.String (fromString)

import Network.Wai.Handler.Warp ( HostPreference, Port
                                , defaultSettings, runSettings
                                , setHost, setPort, setTimeout
                                )
import System.Console.CmdArgs ( Data, Typeable
                              , cmdArgs, help, opt, summary, typ, (&=)
                              )

import Web.Feed.FeedTranslatorWai (application)

data FeedTranslator = FeedTranslator { host :: String
                                     , port :: Port
                                     , timeout :: Int
                                     } deriving (Show, Data, Typeable)

main :: IO ()
main = do
    args <- cmdArgs $
        FeedTranslator { host = "0.0.0.0" &= typ "HOST"
                                          &= help "host to listen [0.0.0.0]"
                       , port = 8080 &= typ "PORT"
                                     &= help "port to listen [8080]"
                       , timeout = 30 &= typ "SECONDS"
                                      &= help "timeout in seconds [30]"
                       } &= summary "Translate syndication feeds"
    app <- application
    let settings = setHost (fromString $ host args) $
                   setPort (port args) $
                   setTimeout (timeout args) defaultSettings
    putStrLn ("Running on http://" ++ host args ++ ":" ++ show (port args) ++
              "/ (Press CTRL+C to quit)")
    runSettings settings app
