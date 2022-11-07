module LexicalAnalyzer (autoParentheses) where

autoParentheses :: String -> String
autoParentheses s = case break (=='=') (dropWhile (==' ') s) of
    (expr, []) -> formatExpr expr
    (decl, expr) -> trim decl ++ " = " ++ (formatExpr . dropWhile (==' ') . tail) expr

formatExpr :: String -> String
formatExpr str = "(" ++ auPe str ++ ")"
    where 
        auPe (s:ss) 
            | s == ' ' = let nextToken = dropWhile (==' ') ss in if null nextToken then [] else ' ' : auPe nextToken
            | s == '.' = let (inScope, outerScope) = (findInScope 0 [] . dropWhile (==' ')) ss in 
                ". " ++ formatExpr inScope ++ auPe outerScope
            | otherwise = s : auPe ss
        auPe [] = ""

        findInScope :: Int -> String -> String -> (String, String)
        findInScope _ ins [] = (reverse ins, [])
        findInScope 0 ins (')':ss) = (reverse ins, ')':ss)
        findInScope n ins (s:ss) = case s of
            ')' -> findInScope (n - 1) (')':ins) ss
            '(' -> findInScope (n + 1) ('(':ins) ss
            _ -> findInScope n (s:ins) ss

trim :: String -> String
trim [] = []
trim (s:ss) = case s of
    ' ' -> let nextToken = dropWhile (==' ') ss in if null nextToken then [] else ' ' : trim nextToken
    _ -> s : trim ss
