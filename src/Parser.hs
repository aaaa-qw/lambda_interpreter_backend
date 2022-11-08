module Parser where

import Text.Parsec
import Data.Functor.Identity ( Identity )
import Control.Applicative hiding (many)

data Program = Decl String Expr | ProgE Expr | PNoCnt deriving (Eq, Show)
data Expr = AppE Expr Expr'| IdE String | Funct Params Expr deriving (Eq, Show)
data Expr' = AppE' Expr Expr' | E'NoCnt deriving (Eq, Show)
newtype Params = Params [String] deriving (Eq, Show)

ids :: ParsecT String u Identity [Char]
ids = (:) <$> (letter :: ParsecT String u Identity Char) <*> many alphaNum

ws :: ParsecT String u Identity Char
ws = char ' '

optWs :: ParsecT String u Identity ()
optWs = Text.Parsec.optional (char ' ')

parseProgram :: ParsecT String u Identity Program
parseProgram = choice  [
        eof >> return PNoCnt,
        string "let" >> ws >> (Decl <$> ids) <*> (string " = " >> parseExpr),
        ProgE <$> parseExpr
    ]

parseExpr :: ParsecT String u Identity Expr
parseExpr = choice [
        char '(' >> optWs >> (AppE <$> parseExpr <*> (optWs >> parseExpr')),
        string "lambda" >> ws >> (Funct <$> parseParams <*> (char '.' >> optWs >> parseExpr)),
        IdE <$> ids
    ]

parseExpr' :: ParsecT String u Identity Expr'
parseExpr' = choice [
        (char ')' >> return E'NoCnt) <* optWs,
        AppE' <$> parseExpr <*> (optWs >> parseExpr')
    ]

parseParams :: ParsecT String u Identity Params
parseParams = Params <$> some (ids <* optWs)

parse :: String -> Program
parse s = case Text.Parsec.parse parseProgram "" s of
    Right program -> program
    Left v -> Decl (show v) (IdE "a")