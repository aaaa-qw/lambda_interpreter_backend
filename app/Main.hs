{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
    import Network.Wai.Handler.Warp (run)
    import WebService (app1)
    
    main :: IO ()
    main = run 8080 app1
