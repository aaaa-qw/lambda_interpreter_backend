module LexicalAnalyzer (autoParentheses) where

autoParentheses :: String -> String
autoParentheses s = case break (=='=') (dropWhile (==' ') s) of
    (expr, []) -> "(" ++ expr ++ ")"
    (decl, expr) -> decl ++ "=" ++ "(" ++ (dropWhile (==' ') . tail) expr ++ ")"
