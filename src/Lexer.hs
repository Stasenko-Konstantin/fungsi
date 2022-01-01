{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lexer where

import Data.Char
import Debug.Trace
import Help
import Token

scan :: String -> [Token]
scan [] = []
scan source = help [] source
  where
    help :: [Token] -> String -> [Token]
    help tokens "" = reverse $ Token EOF "eof" None : tokens
    help tokens (':' : '=' : ode) = help (Token BIND ":=" InfixOp : tokens) ode
    help tokens code@(c : ode) =
      case c of
        '+' -> help (Token PLUS "+" InfixOp : tokens) ode
        '*' -> help (Token STAR "*" InfixOp : tokens) ode
        '^' -> help (Token POWER "^" InfixOp : tokens) ode
        '|' -> help (Token VBAR "|" InfixOp : tokens) ode
        ',' -> help (Token COMMA "," InfixOp : tokens) ode
        '\n' -> help (simpleToken SEMICOLON "\n" : tokens) ode
        '-' ->
          if head ode == '>'
            then help (simpleToken LAMBDA "->" : tokens) $ tail ode
            else help (Token MINUS "-" InfixOp : tokens) ode
        _
          | c == '/' || c == '\\' || c == '!' ->
            if head ode == '='
              then help (Token NEQUAL "/=" InfixOp : tokens) $ slice ode 1 $ length ode
              else help (Token SLASH "/" InfixOp : tokens) ode
        _ | c == '(' -> help (simpleToken LPAREN "(" : tokens) ode
        _ | c == ')' -> help (simpleToken RPAREN ")" : tokens) ode
        _ | c == '[' -> help (simpleToken LCPAREN "[" : tokens) ode
        _ | c == ']' -> help (simpleToken RCPAREN "]" : tokens) ode
        _ | c == '{' -> help (simpleToken LBRACE "{" : tokens) ode
        _ | c == '}' -> help (simpleToken RBRACE "}" : tokens) ode
        _ | c == '=' || c == '<' || c == '>' -> addEqual code
        _ | isNumber c -> addNum code False ""
        _ | isLetter c -> addName code
        _ | c == ' ' || c == '\t' || c == '\r' -> help tokens ode
        _ | otherwise -> error $ "Syntax error: " ++ show c
      where
        simpleToken :: TokenType -> String -> Token
        simpleToken token content = Token token content None

        addEqual :: String -> [Token]
        addEqual ('<' : '=' : xs) = help (Token LEQUAL "<=" InfixOp : tokens) xs
        addEqual ('>' : '=' : xs) = help (Token GEQUAL ">=" InfixOp : tokens) xs
        addEqual ('=' : '=' : xs) = help (Token DEQUAL "==" InfixOp : tokens) xs
        addEqual ('<' : xs) = help (Token LESS "<" InfixOp : tokens) ode
        addEqual ('>' : xs) = help (Token GREAT ">" InfixOp : tokens) ode
        addEqual ('=' : xs) = help (Token EQUAL "=" InfixOp : tokens) ode

        addNum :: String -> Bool -> String -> [Token]
        addNum ('.' : xs) isFloat res = addNum xs True (res ++ ".")
        addNum xs@(x : s) isFloat res
          | isNumber x = addNum s isFloat (res ++ [x])
          | otherwise = resultNum isFloat res xs
        addNum [] isFloat res = resultNum isFloat res []

        resultNum :: Bool -> String -> String -> [Token]
        resultNum True res next = help (Token FLOAT res Num : tokens) next
        resultNum False res next = help (Token INT res Num : tokens) next

        addName code = help (Token (fst4 token) (snd4 token) (thd4 token) : tokens) (fth4 token)
          where
            token = case code of
              ('i' : 'f' : xs) -> (IF, "if", Name, xs)
              ('t' : 'h' : 'e' : 'n' : xs) -> (THEN, "then", None, xs)
              ('e' : 'l' : 's' : 'e' : xs) -> (ELSE, "else", None, xs)
              ('t' : 'r' : 'u' : 'e' : xs) -> (TRUE, "true", Name, xs)
              ('f' : 'a' : 'l' : 's' : 'e' : xs) -> (FALSE, "false", Name, xs)
              ('a' : 'n' : 'd' : xs) -> (AND, "and", Name, xs)
              ('o' : 'r' : xs) -> (OR, "or", Name, xs)
              ('n' : 'o' : 't' : xs) -> (NOT, "not", Name, xs)
              ('n' : 'i' : 'l' : xs) -> (NIL, "nil", Name, xs)
              xs ->
                ( NAME,
                  takeWhile isName code,
                  Name,
                  dropWhile isName xs
                )

isName :: Char -> Bool
isName c = isLetter c || isNumber c