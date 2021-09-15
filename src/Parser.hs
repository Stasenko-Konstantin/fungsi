module Parser where

import Token
import Help

data Expr = Expr {expr :: Expr}
          | Next {left :: Expr, right :: Expr}
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
    (Token _ _ None) -> parseError
    where
        parseNum        = undefined
        parseName       = undefined
        parseGroup      = undefined
        parseLambda     = undefined
        parseUnaryMinus = Prefix t (parse okens)

        takeTokens :: [Token] -> Token -> [Token]
        takeTokens ts t = if not $ t `elem` ts then parseError else
                          takeWhile (== t) ts

        parseError = error "Parse error"