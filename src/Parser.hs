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