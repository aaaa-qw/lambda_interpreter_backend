module Grammar (Program (..), Expr (..)) where

    data Program = Decl String Expr1 | ProgE Expr deriving (Eq, Show)
    data Expr = Id String Expr| E Expr1 Expr | Fun Params Expr1 Expr | ENoCnt deriving (Eq, Show)
    type Expr1 = Expr
    type Params = [String]