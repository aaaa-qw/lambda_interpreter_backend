module Parser where

data Program = Decl String Expr | ProgE Expr deriving (Eq, Show)
data Expr = AppE Expr Expr'| IdE String | Funct Params Expr deriving (Eq, Show)
data Expr' = AppE' Expr Expr' | E'NoCnt deriving (Eq, Show)
newtype Params = Params [String] deriving (Eq, Show)

parse :: String -> Program
parse s = undefined