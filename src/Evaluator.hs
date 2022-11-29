{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Evaluator (replaceUnbound, betaReduce, evaluate, EvaluationError(..)) where

    import Control.Monad.State.Lazy
    import Parser ( Expr(..), Program(..))
    import Data.List ( union )
    import Data.Map (Map)
    import qualified Data.Map as Map

    replaceUnbound :: Map String Expr -> Expr -> Expr
    replaceUnbound tableSym expression = evalState (replaceUnbound' expression) (0, [])
        where
        replaceUnbound' :: Expr -> State (Int, [String]) Expr
        replaceUnbound' (Id val expr) = do
            (uniqueNum, localDef) <- get
            if val `elem` localDef
                then
                    Id val <$> replaceUnbound' expr
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


    type Message = String
    data EvaluationError = ExceedMaxEval Message | VariableAlreadyDefined Message deriving (Eq, Show)

    evaluate :: Int -> Map String Expr -> Program -> Either EvaluationError Program
    evaluate maxEval tableSym program =
        case program of

            (Decl newVar nonEmptyExpr) -> case Map.lookup newVar tableSym of
                Just expr' -> Left $ VariableAlreadyDefined $ newVar ++ " is already defined as " ++ show expr'
                Nothing -> case (betaReduce maxEval . replaceUnbound tableSym) nonEmptyExpr of
                    Right expr' -> Right $ Decl newVar expr'
                    Left msg -> Left (ExceedMaxEval msg) 

            (ProgE expr) -> case (betaReduce maxEval . replaceUnbound tableSym) expr of
                Right expr' -> Right $ ProgE expr'
                Left msg -> Left (ExceedMaxEval msg)


    type IsReduceable = Bool
    betaReduce :: Int -> Expr -> Either Message Expr
    betaReduce maxEval expression = evalState (betaReduce' expression) (0, True)
        where
            
            merge :: Expr -> Expr -> Expr
            merge (Id val ENoCnt) e2 = Id val e2
            merge (E e1 ENoCnt) e2 = E e1 e2
            merge (Fun params body ENoCnt) e2 = Fun params body e2
            merge e1 e2 = E e1 e2

            betaReduce' :: Expr -> State (Int, IsReduceable) (Either Message Expr)

            betaReduce' expr1 = do
                (step1, _) <- get
                if step1 > maxEval then return $ Left "Exceed maximum function application"
                else 
                    case expr1 of
                        (Id val expr) ->
                            do
                            (step, _) <- get
                            put (step, False)
                            mexpr <- betaReduce' expr
                            case mexpr of
                                Left msg -> return (Left msg)
                                Right expr' -> return $ Right $ Id val expr'
                    
                        ENoCnt -> return $ Right ENoCnt
                    
                        (E e1 ENoCnt) -> do 
                            (step, _) <- get
                            put (step, True)
                            betaReduce' e1 
                    
                        (E e1 e2) -> do
                            (oldStep, oldState) <- get
                            put (oldStep, True)
                            me1 <- betaReduce' e1
                            (newStep, _) <- get
                            put (newStep, oldState)
                            case me1 of
                                Left msg -> return $ Left msg
                                Right e1' -> case merge e1' e2 of
                                    (E e1'' e2') -> do
                                        me2 <- betaReduce' e2'
                                        case me2 of
                                            Left msg -> return $ Left msg
                                            Right e2'' -> return $ Right $ E e1'' e2''
                                    fun@(Fun {}) -> betaReduce' fun
                                    expr -> put (newStep, False) >> betaReduce' expr

                        (Fun params body e2) -> do
                            (step, isReduce) <- get
                            if not isReduce then 
                                do
                                    mbody <- betaReduce' body
                                    case mbody of
                                        Left msg -> return $ Left msg
                                        Right body' ->
                                            do
                                                (newStep, _) <- get
                                                put (newStep, isReduce)
                                                me2 <- betaReduce' e2
                                                case me2 of 
                                                    Left msg -> return $ Left msg
                                                    Right e2' -> return $ Right $ Fun params body' e2'
                            else
                                if e2 == ENoCnt then 
                                    do
                                        mbody <- betaReduce' body
                                        case mbody of
                                            Left msg -> return $ Left msg
                                            Right body' -> return $ Right $ Fun params body' ENoCnt
                                else
                                    do
                                        put (step+1, isReduce)
                                        case params of
                                            [x] -> betaReduce' (E (subs x (inner e2) body) (outer e2))
                                            (x:xs) -> betaReduce' $ Fun xs (subs x (inner e2) body) (outer e2) 
                                        where
                                            subs x repl (Id val b) = if x == val then merge repl (subs x repl b)
                                                                        else Id val (subs x repl b)
                                            subs _ _ ENoCnt = ENoCnt
                                            subs x repl (E e1 e2') = merge (subs x repl e1) (subs x repl e2')
                                            subs x repl (Fun ps b e2') = if x `elem` ps then Fun ps b (subs x repl e2')
                                                                        else Fun ps (subs x repl b) (subs x repl e2')

                                            inner (Id val _) = Id val ENoCnt
                                            inner (E e1 _) = e1
                                            inner (Fun p b _) = Fun p b ENoCnt

                                            outer (Id _ e2') = e2'
                                            outer (E _ e2') = e2'
                                            outer (Fun _ _ e2') = e2'