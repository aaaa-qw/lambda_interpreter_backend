{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
    import Network.Wai.Handler.Warp (run)
    import Network.Wai (Middleware)
    import WebService (app1)
    import Network.Wai.Middleware.Cors
    import System.Environment
    
    main :: IO ()
    main = do
        mfrontend_url = lookupEnv "FRONTEND_URL"
        case mfrontend_url of
            Just url -> run 8080 $ (corsPolicy url) app1
            Nothing  -> run 8080 % (corsPolicy "") app1

    corsPolicy :: String -> Middleware
    corsPolicy frontend_url = cors (const $ Just policy)
        where
            policy = simpleCorsResourcePolicy
                { 
                    corsMethods = [ "GET", "POST", "PUT", "OPTIONS" ],
                    corsOrigins = Just ([frontend_url, "http://localhost:3000"], True),
                    corsRequestHeaders = [ "Content-Type" ]
                }
