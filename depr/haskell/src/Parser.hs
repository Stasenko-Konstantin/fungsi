{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser where

import Help
import Token

data Expr
  = Next {left :: Expr, right :: Expr}
  | Prefix {op :: Token, expr :: Expr}
  | Infix {left :: Expr, op :: Token, right :: Expr}
  | Postfix {expr :: Expr, op :: Token}
  | Lambda {params :: Expr, body :: Expr}
  | Group {expr :: Expr}
  | Term {term :: Token}
  | Eof {eof :: Token}
  deriving (Show)

parse :: [Token] -> Expr
parse t@[Token EOF _ _] = Eof $ head t
parse tokens@(t : okens) = case t of
  (Token _ _ Num) -> Term t
  (Token _ _ Name) -> parseName
  (Token _ "(" _) -> parseGroup tokens
  (Token _ "->" _) -> parseLambda
  (Token _ "-" _) -> parseUnaryMinus
  (Token _ _ None) -> parseError
  where
    parseNum = undefined
    parseName = undefined

    parseLambda = undefined
    parseUnaryMinus = Prefix t (parse okens)

    takeTokens :: [Token] -> Token -> [Token]
    takeTokens ts t =
      if t `notElem` ts
        then parseError
        else takeWhile (== t) ts

    parseError = error "Parse error"

parseGroup :: [Token] -> Expr
parseGroup tokens = HELP tokens 0 0 0 0 []
  where
    HELP :: [Token] -> Int -> Int -> Int -> Int -> [Token] -> Expr -- endn - индекс конца Group, end - Токены после )
    HELP [] push pop n endn end =
      if n == length tokens
        then Group $ parse $ slice tokens 1 (endn - 1)
        else Next (Group $ parse $ slice tokens 1 (endn - 1)) (parse end)
    HELP ((Token NIL _ _) : ts) push pop n endn end = HELP ts push (pop + 1) (n + 1) endn end
    HELP ((Token _ "(" _) : ts) push pop n endn end = HELP ts (push + 1) pop (n + 1) endn end
    HELP ((Token _ ")" _) : ts) push pop n endn end = HELP ts push (pop + 1) (n + 1) n ts
    HELP (t : ts) push pop n endn end = HELP ts push pop (n + 1) endn end
