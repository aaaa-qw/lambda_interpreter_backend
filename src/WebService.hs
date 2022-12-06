{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module WebService (app1) where

    import Data.Map (Map)
    import qualified Data.Map as Map
    import GHC.Generics (Generic)
    import Data.Aeson (ToJSON, FromJSON)
    import Interpreter (execute)
    import Servant.API (ReqBody, Post, JSON, (:>))
    import Servant.Server (Handler, serve)
    import Network.Wai ( Application )
    import Data.Maybe ( fromMaybe )
    import Servant (Proxy (Proxy))

    

    type InterpreterAPI = "evaluate" :> ReqBody '[JSON] ExpressionInfo :> Post '[JSON] EvaluationReport

    data EvaluationReport = MkReport {
        result:: String,
        errorMessages :: [String]
    } deriving Generic

    data ExpressionInfo = EvaluationReport {
        expression :: String,
        predefined_vars :: Maybe (Map String String),
        maximumEval :: Maybe Int
    } deriving Generic

    instance ToJSON EvaluationReport
    instance FromJSON ExpressionInfo

    evalHandler :: ExpressionInfo -> Handler EvaluationReport
    evalHandler expressionInfo = 
        let (errs, res) = execute (fromMaybe Map.empty (predefined_vars expressionInfo)) (fromMaybe 1000 (maximumEval expressionInfo)) (expression expressionInfo)
        in return (MkReport {result=res, errorMessages=errs})
    
    interpreterAPI :: Proxy InterpreterAPI
    interpreterAPI = Proxy

    app1 :: Application
    app1 = serve interpreterAPI evalHandler