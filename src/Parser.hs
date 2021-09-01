module Parser where

import Token
import Help

data Expr = Expr {left :: Expr, right :: Expr}
          | Prefix {op :: Token, expr :: Expr}
          | Infix {left :: Expr, op :: Token, right :: Expr}
          | Postfix {expr :: Expr, op :: Token}
          | Lambda {params :: Expr, body :: Expr}
          | Term {term :: Token}    deriving Show

parse :: [Token] -> Expr
parse tokens@(t:okens) = case t of
    (Token _ _ Num)  -> parseNum okens
    (Token _ _ Name) -> parseName
    (Token _ "(" _)  -> parseGroup
    (Token _ "@" _)  -> parseLambda
    (Token _ "-" _)  -> parseUnaryMinus
    (Token _ _ None) -> error "Parse error"
    where
        parseNum        = undefined
        parseName       = undefined
        parseGroup      = undefined
        parseLambda     = undefined
        parseUnaryMinus = Prefix t (parse okens)