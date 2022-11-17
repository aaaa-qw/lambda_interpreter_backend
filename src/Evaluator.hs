module Evaluator (replaceUnbound) where

    import Control.Monad.State.Lazy
    import Parser ( Expr(..) )
    import Data.List ( union )
    import Data.Map (Map)
    import qualified Data.Map as Map
    
    
    replaceUnbound :: Expr -> Map String Expr -> Expr
    replaceUnbound expr tableSym = evalState (replaceUnbound' expr) (0, [])
        where
        replaceUnbound' :: Expr -> State (Int, [String]) Expr
        replaceUnbound' (Id val expr) = do
            (uniqueNum, localDef) <- get
            if val `elem` localDef
                then
                    put (uniqueNum, localDef) >> Id val <$> replaceUnbound' expr
                else
                    case Map.lookup val tableSym of
                        Nothing -> put (uniqueNum + 1, localDef) >> Id ('x':'\'': show uniqueNum) <$> replaceUnbound' expr
                        Just e -> E <$> replaceUnbound' e <*> replaceUnbound' expr
        replaceUnbound' ENoCnt = return ENoCnt
        replaceUnbound' (E expr1 expr2) = do
            (_, localDef) <- get
            expr1' <- replaceUnbound' expr1
            
            -- reset local definition
            (newUniqueNum, _) <- get
            put (newUniqueNum, localDef)
            E expr1' <$> replaceUnbound' expr2

        replaceUnbound' (Fun params expr outerExpr) = do
            (uniqueNum, localDef) <- get
            
            -- replace unbound in function body
            put (uniqueNum, params `union` localDef)
            expr' <- replaceUnbound' expr

            -- reset local definition 
            (newUniqueNum, _) <- get
            put (newUniqueNum, localDef)

            Fun params expr' <$> replaceUnbound' outerExpr