{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parser where

import Text.Parsec
import Data.Functor.Identity ( Identity )
import Control.Applicative (liftA3, liftA2)

data Program = Decl String Expr1 | ProgE Expr deriving (Eq, Show)
data Expr = Id String Expr| E Expr1 Expr | Fun Params Expr1 Expr | ENoCnt deriving (Eq, Show)
type Expr1 = Expr
type Params = [String]

ids :: ParsecT String u Identity [Char]
ids = (:) <$> (letter :: ParsecT String u Identity Char) <*> many alphaNum

optWs :: ParsecT String u Identity ()
optWs = optional (char ' ')

functSym :: ParsecT String u Identity String
functSym = choice [
        string "lambda" <* char ' ',
        string "\\"
    ]

parseFunc :: ParsecT String u Identity Expr
parseFunc = functSym >> liftA3 Fun parseParams (char '.' >> parseExpr1) parseExpr

parseParams :: ParsecT String u Identity [String]
parseParams = many (ids <* optWs)

parseId :: ParsecT String u Identity Expr
parseId = liftA2 Id ids (optWs >> parseExpr)

parseE :: ParsecT String u Identity Expr
parseE = liftA2 E parseExpr1 parseExpr

parseExpr1 :: ParsecT String u Identity Expr
parseExpr1 = choice [
        parseId,
        char '(' >> (parseFunc <|> parseE)
    ]

parseExpr :: ParsecT String u Identity Expr
parseExpr = choice [
        parseId,
        char '(' >> (parseFunc <|> parseE),
        char ')' >> return ENoCnt,
        eof >> return ENoCnt
    ]

parseDecl :: ParsecT String u Identity Program
parseDecl = liftA2 Decl (string "let " >> ids) (char '=' >> parseExpr1)

parseProgram :: ParsecT String u Identity Program
parseProgram = choice [
        parseDecl,
        ProgE <$> parseExpr
    ]

parse :: String -> Either ParseError Program
parse = Text.Parsec.parse parseProgram ""

unParse :: Expr -> String
unParse p = case p of
    ENoCnt -> ""
    expr -> (dropWhile (==' ') . unParseE [""] "") expr
    where
        unParseE :: [String] -> String -> Expr -> String
        unParseE acc last' ENoCnt = (formatter . unwords . reverse) (last':acc)
        unParseE acc lst (Id val e2) = unParseE (val:acc) lst e2
        unParseE acc lst (Fun params body e2) = unParseE (unParseE [[]] ")" body : "." : ("(\955" ++ unwords params) : acc) lst e2
        unParseE acc lst (E (Id val ENoCnt) e2) = unParseE (val:acc) lst e2
        unParseE acc lst (E (Fun params body ENoCnt) e2) = unParseE acc lst (Fun params body e2)
        unParseE acc lst (E (E e1' ENoCnt) e2) = unParseE acc lst (E e1' e2)
        unParseE acc lst (E e1 e2) = unParseE (unParseE ["("] ")" e1:acc) lst e2

        --Will remove unnecassary whitespace and parenthesis
        formatter :: String -> String
        formatter = foldr op ""
            where
                op :: Char -> String -> String
                op ' ' [] = []
                op ' ' b = if (not . null) b && head b `elem` " ().\955" then b else ' ':b
                op s b = if s `elem` "().\955" then s:dropWhile (==' ') b
                            else s:b
