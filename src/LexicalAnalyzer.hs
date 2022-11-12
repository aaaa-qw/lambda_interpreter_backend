{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module LexicalAnalyzer (autoParentheses) where

specialSymbol :: String
specialSymbol = "\\.()="


autoParentheses :: String -> String
autoParentheses str = case break (=='=') (dropWhile (==' ') str) of
    ([], []) -> ""
    (expr, []) -> (reverse . removeAndAddLast "(" (Just ')')) expr
    (decl, expr) -> (reverse . removeAndAddLast "" Nothing) decl ++ '=':(reverse . removeAndAddLast "(" (Just ')')) (tail expr)
    where
        removeAndAddLast :: String -> Maybe Char -> String -> String
        removeAndAddLast acc Nothing [] = acc
        removeAndAddLast acc (Just a) [] = a:acc
        removeAndAddLast acc mc (s:ss) = 
            case s of
                ' ' -> let rest = dropWhile (==' ') ss in 
                    if  null rest 
                            || head rest `elem` specialSymbol 
                            || (not . null) acc && head acc `elem` specialSymbol 
                        then removeAndAddLast acc mc rest
                    else removeAndAddLast (s:acc) mc rest
                _ -> removeAndAddLast (s:acc) mc ss