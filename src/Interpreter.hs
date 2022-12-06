{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Interpreter (execute) where

    import Data.Map (Map)
    import qualified Data.Map as Map
    import LexicalAnalyzer (autoParentheses)

    import qualified Parser as P

    import qualified TableSymbolParser as T

    import Evaluator ( EvaluationError(..), evaluate)
    import Data.Either (isLeft)
    import Grammar (Program (..), Expr (..))


    type Expression = String
    type ErrorMessages = [String]
    execute :: Map String String -> Int -> Expression -> (ErrorMessages, String)
    execute tableSym maxFunApplication expression =
        case (P.parse . autoParentheses) expression of
                Left msg -> ([msg], "")
                Right expr' ->
                    let
                        parsedTabSym = T.parseExpression . autoParentheses <$> tableSym
                    in case Map.foldrWithKey opK [""] parsedTabSym of
                        [[]] -> (postProcess . evaluate maxFunApplication (Map.map (\(Right a) -> a) parsedTabSym)) expr'
                        msg -> let (err, res) = (postProcess . evaluate maxFunApplication Map.empty) expr'
                            in (msg ++ "User-Defined variable is not used":err, res)

                    where
                        opK k a b' = if isLeft a then ("Expression of variable "++ k ++ " can not be parsed") : b' else b'

                        getExpr :: Program -> Expr
                        getExpr (ProgE e) = e
                        getExpr (Decl _ e) = e

                        postProcess :: Either EvaluationError Program -> ([String], String)
                        postProcess s = case s of
                            Left (ExceedMaxEval msg) -> ([msg], "")
                            Left VariableAlreadyDefined -> (["Variable is already defined"], "")
                            Right program -> ([[]], (P.unParse . getExpr) program)