{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
    import Network.Wai.Handler.Warp (run)
    import Network.Wai (Middleware)
    import WebService (app1)
    import Network.Wai.Middleware.Cors    
    main :: IO ()
    main = run 8080 $ corsPolicy app1

    corsPolicy :: Middleware
    corsPolicy = cors (const $ Just policy)
        where
            policy = simpleCorsResourcePolicy
                { 
                    corsMethods = [ "GET", "POST", "PUT", "OPTIONS" ],
                    corsOrigins = Just (["http://localhost:3000", "https://lambdainterpreterfrontend-production-42ea.up.railway.app/"], True),
                    corsRequestHeaders = [ "content-type" ]
                }