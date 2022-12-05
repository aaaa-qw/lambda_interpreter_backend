module TableSymbolParser (parseExpression) where

import Text.Parsec
import Data.Functor.Identity ( Identity )
import Control.Applicative (liftA3, liftA2)
import Grammar ( Expr(..)) 
import Text.Parsec.Error (messageString, errorMessages)

ids :: ParsecT String u Identity [Char]
ids = (:) <$> (letter :: ParsecT String u Identity Char) <*> many (alphaNum <|> char '\'')

optWs :: ParsecT String u Identity ()
optWs = optional (char ' ')

functSym :: ParsecT String u Identity String
functSym = choice [
        string "lambda" <* char ' ',
        string "\\"
    ]

parseFunc :: ParsecT String u Identity Expr
parseFunc = functSym >> liftA3 Fun parseParams (char '.' >> parseExpr1 <* char ')') parseExpr

parseParams :: ParsecT String u Identity [String]
parseParams = many (ids <* optWs)

parseId :: ParsecT String u Identity Expr
parseId = liftA2 Id ids (optWs >> parseExpr)

parseE :: ParsecT String u Identity Expr
parseE = liftA2 E (parseExpr1 <* char ')') parseExpr

parseExpr1 :: ParsecT String u Identity Expr
parseExpr1 = choice [
        parseId,
        char '(' >> (parseFunc <|> parseE) 
    ]

parseExpr :: ParsecT String u Identity Expr
parseExpr = choice [
        parseId,
        char '(' >> (parseFunc <|> parseE),
        lookAhead (char ')') >> return ENoCnt,
        eof >> return ENoCnt
    ]

type ErrorMessages = String
parseExpression :: String -> Either ErrorMessages Expr
parseExpression str = case Text.Parsec.parse parseExpr "" str of 
    Left parseError -> Left $ unwords (messageString <$> errorMessages parseError)
    Right val -> Right val