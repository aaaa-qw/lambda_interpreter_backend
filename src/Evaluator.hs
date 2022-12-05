{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Evaluator (replaceUnbound, betaReduce, evaluate, EvaluationError(..)) where

    import Control.Monad.State.Lazy
    import Grammar ( Expr(..), Program(..) )
    import Data.Map (Map)
    import qualified Data.Map as Map

    replaceUnbound :: Map String Expr -> Expr -> Expr
    replaceUnbound tableSym expression = evalState (replaceUnbound' expression) ((0, []), (0, Map.empty))
        where
        replaceUnbound' :: Expr -> State ((Int, [String]), (Int, Map String String)) Expr
        replaceUnbound' (Id val expr) = do
            ((varNum, localDef), (funNum, replMap)) <- get
            case Map.lookup val replMap of
                Just newVal -> Id newVal <$> replaceUnbound' expr
                Nothing ->  if val `elem` localDef
                                then
                                    Id val <$> replaceUnbound' expr
                                else
                                    case Map.lookup val tableSym of
                                        Nothing -> put ((varNum + 1, localDef), (funNum, replMap)) >> Id ('x':'\'': show varNum) <$> replaceUnbound' expr
                                        -- Just e -> E <$> replaceUnbound' e <*> replaceUnbound' expr
                                        Just e -> do
                                            put ((varNum, []), (funNum, Map.empty))
                                            e' <- replaceUnbound' e

                                            ((varNum', _), (funNum', _)) <- get
                                            put ((varNum', localDef), (funNum', replMap))
                                            E <$> replaceUnbound' e' <*> replaceUnbound' expr

        replaceUnbound' ENoCnt = return ENoCnt
        replaceUnbound' (E expr1 expr2) = do
            ((_, localDef), (_, replMap)) <- get
            expr1' <- replaceUnbound' expr1

            -- reset local definition
            ((newUniqueNum, _), (newFunNum, _)) <- get
            put ((newUniqueNum, localDef), (newFunNum, replMap))
            E expr1' <$> replaceUnbound' expr2

        replaceUnbound' (Fun [] expr ENoCnt) = replaceUnbound' expr
        replaceUnbound' (Fun params expr outerExpr) = do
            ((uniqueNum, localDef), (funNum, replMap)) <- get

            -- replace unbound in function body
            if head params `elem` localDef then
                do
                    put ((uniqueNum, localDef), (funNum + 1, Map.insert (head params) ('x':show funNum) replMap))
                    expr' <- replaceUnbound' (Fun (tail params) expr ENoCnt)

                    -- reset local definition 
                    ((newUniqueNum, _), (newFunNum, _)) <- get
                    put ((newUniqueNum, localDef), (newFunNum, replMap))
                    Fun ['x':show funNum] expr' <$> replaceUnbound' outerExpr
            else
                do
                    put ((uniqueNum, head params:localDef), (funNum, replMap))
                    expr' <- replaceUnbound' (Fun (tail params) expr ENoCnt)

                    -- reset local definition 
                    ((newUniqueNum, _), (newFunNum, _)) <- get
                    put ((newUniqueNum, localDef), (newFunNum, replMap))
                    Fun [head params] expr' <$> replaceUnbound' outerExpr
            -- replaceUnbound' expr
            -- put ((uniqueNum, params `union` localDef), calculateNewState localDef params funNum replMap)
            -- (_, (_, newReplMap)) <- get
            -- expr' <- replaceUnbound' expr

            -- -- reset local definition 
            -- ((newUniqueNum, _), (newFunNum, _)) <- get
            -- put ((newUniqueNum, localDef), (newFunNum, replMap))

            -- Fun (map (\x -> Data.Maybe.fromMaybe x (Map.lookup x newReplMap)) params) expr' <$> replaceUnbound' outerExpr


        -- calculateNewState :: [String] -> [String] -> Int -> Map String String -> (Int, Map String String)
        -- calculateNewState localDef params funNum replMap = let
        --         needToReplcParam = params `intersect` localDef
        --         newReplMap = Map.unionWith (\ _ x -> x) replMap (Map.fromList((fmap . fmap) op (zip needToReplcParam [0..length needToReplcParam - 1])))
        --     in (funNum + length needToReplcParam , newReplMap)
        --         where
        --             op i = "x" ++ show (funNum + i)

    type Message = String
    data EvaluationError = ExceedMaxEval Message | VariableAlreadyDefined deriving (Eq, Show)

    evaluate :: Int -> Map String Expr -> Program -> Either EvaluationError Program
    evaluate maxEval tableSym program =
        case program of

            (Decl newVar nonEmptyExpr) -> case Map.lookup newVar tableSym of
                Just _ -> Left VariableAlreadyDefined
                Nothing -> case (betaReduce maxEval . replaceUnbound tableSym) nonEmptyExpr of
                    Right expr' -> Right $ Decl newVar expr'
                    Left msg -> Left (ExceedMaxEval msg)

            (ProgE expr) -> case (betaReduce maxEval . replaceUnbound tableSym) expr of
                Right expr' -> Right $ ProgE expr'
                Left msg -> Left (ExceedMaxEval msg)


    type IsReduceable = Bool
    type BoundedVars = [String]
    betaReduce :: Int -> Expr -> Either Message Expr
    betaReduce maxEval expression = evalState (betaReduce' expression) (0, True, (0, [""], Map.empty))
        where

            merge :: Expr -> Expr -> Expr
            merge (Id val ENoCnt) e2 = Id val e2
            merge (E e1 ENoCnt) e2 = E e1 e2
            merge (Fun params body ENoCnt) e2 = Fun params body e2
            merge e1 e2 = E e1 e2

            betaReduce' :: Expr -> State (Int, IsReduceable, (Int, BoundedVars, Map String String)) (Either Message Expr)

            betaReduce' expr1 = do
                (step1, _, _) <- get
                if step1 > maxEval then return $ Left "Exceed maximum function application"
                else
                    case expr1 of
                        (Id val expr) ->
                            do
                            (step, _, s) <- get
                            put (step, False, s)
                            mexpr <- betaReduce' expr
                            case mexpr of
                                Left msg -> return (Left msg)
                                Right expr' -> return $ Right $ Id val expr'

                        ENoCnt -> return $ Right ENoCnt

                        (E e1 e2) -> do
                            (oldStep, oldState, boundedState) <- get
                            put (oldStep, True, boundedState)
                            me1 <- betaReduce' e1
                            (newStep, _, boundedState') <- get
                            put (newStep, oldState, boundedState')
                            case me1 of
                                Left msg -> return $ Left msg
                                Right e1' -> case merge e1' e2 of
                                    (E e1'' e2') -> do
                                        me2 <- betaReduce' e2'
                                        case me2 of
                                            Left msg -> return $ Left msg
                                            Right e2'' -> return $ Right $ E e1'' e2''
                                    expr -> betaReduce' expr


                        (Fun [x] body e2) -> do
                            (step, isReduce, (uniqueNum, boundedVars, replMap)) <- get
                            -- if x `elem` boundedVars then
                            --     put (step, isReduce,
                            --         (uniqueNum + 1, "y'":show uniqueNum:boundedVars, Map.insert x ("y'" ++ show uniqueNum) replMap))
                            -- else
                            --     put (step, isReduce, (uniqueNum, x:boundedVars, replMap))
                        
                            if not isReduce then
                                do
                                    mbody <- betaReduce' body
                                    case mbody of
                                        Left msg -> return $ Left msg
                                        Right body' ->
                                            do
                                                --This 2 following line of codes can be commented as function body is always non-empty 
                                                --expression hence Evaluator at least have evaluated 1 identifier so isReduce in this current state 
                                                --is always False
                                                (newStep, _, boundedState') <- get
                                                put (newStep, False, boundedState')

                                                me2 <- betaReduce' e2
                                                case me2 of
                                                    Left msg -> return $ Left msg
                                                    Right e2' -> return $ Right $ Fun [x] body' e2'
                            else
                                if e2 == ENoCnt then
                                    do
                                        mbody <- betaReduce' body
                                        case mbody of
                                            Left msg -> return $ Left msg
                                            Right body' -> return $ Right $ Fun [x] body' ENoCnt
                                else
                                    do
                                        -- put (step+1, isReduce)
                                        let (e2', (uniqueNum', _)) = runState (replDupBounded (getBoundedVars [] (Fun [x] body ENoCnt)) (inner e2)) (uniqueNum, Map.empty)
                                        put (step + 1, isReduce, (uniqueNum', boundedVars, replMap))
                                        betaReduce' (E (subs x e2' body) (outer e2))
                                        -- case params of
                                        --     [x] -> betaReduce' (E (subs x (inner e2) body) (outer e2))
                                        --     (x:xs) -> betaReduce' $ Fun xs (subs x (inner e2) body) (outer e2)
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

                                            getBoundedVars :: [String] -> Expr -> [String]
                                            getBoundedVars acc (Fun [x] body' _) = getBoundedVars (x:acc) body'
                                            getBoundedVars acc _ = acc

                                            replDupBounded :: [String] -> Expr -> State (Int, Map String String) Expr
                                            replDupBounded localDef (Id val e) = do
                                                (_, replMap) <- get
                                                case Map.lookup val replMap of
                                                    Nothing -> Id val <$> replDupBounded localDef e
                                                    Just a -> Id a <$> replDupBounded localDef e
                                            replDupBounded _ ENoCnt = return ENoCnt
                                            replDupBounded localDef (E e1 e2') =
                                                E <$> replDupBounded localDef e1 <*> replDupBounded localDef e2'
                                            replDupBounded localDef (Fun [x'] body' e1) =
                                                if x' `elem` localDef then
                                                    do
                                                        (uniqueNum, replMap) <- get
                                                        put (uniqueNum + 1, Map.insert x' ("y" ++ show uniqueNum) replMap)
                                                        Fun ["y" ++ show uniqueNum] <$> replDupBounded localDef body' <*> replDupBounded localDef e1
                                                else
                                                    Fun [x'] <$> replDupBounded localDef body' <*> replDupBounded localDef e1