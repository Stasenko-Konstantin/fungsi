module Expr where

import Token

data Expr =   Infix   Expr  Token Expr
            | Postfix Expr  Token
            | Prefix  Token Expr
            | Group   Expr
            | Literal Token       deriving Show
