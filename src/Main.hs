import Network.Wai.Handler.Warp (run)

import Web.Feed.FeedTranslatorWai (application)

port :: Int
port = 8080

main :: IO ()
main = do
    app <- application
    putStrLn ("Running on http://0.0.0.0:" ++ show port ++
              "/ (Press CTRL+C to quit)")
    run port app
