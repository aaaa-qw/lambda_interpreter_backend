{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
    import Network.Wai.Handler.Warp (run)
    import Network.Wai (Middleware)
    import WebService (app1)
    import Network.Wai.Middleware.Cors
    import System.Environment
    import qualified Data.ByteString.Char8 as BS
    
    main :: IO ()
    main = do
        mfrontend_url <- lookupEnv "FRONTEND_URL"
        case mfrontend_url of
            Just f_url -> run 8080 $ corsPolicy f_url app1
            Nothing  -> run 8080 $ corsPolicy "" app1

    corsPolicy :: String -> Middleware
    corsPolicy frontend_url = cors (const $ Just policy)
        where
            policy = simpleCorsResourcePolicy
                { 
                    corsMethods = [ "GET", "POST", "PUT", "OPTIONS" ],
                    corsOrigins = Just ([BS.pack frontend_url, "http://localhost:3000"], True),
                    corsRequestHeaders = [ "Content-Type" ]
                }
