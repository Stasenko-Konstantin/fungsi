module Lexer where

import Token

import Data.Char 
import Debug.Trace

scan :: String -> [Token]
scan []     = []
scan source = help [] source
    where
        help :: [Token] -> String -> [Token]
        help tokens ""           = reverse tokens
        help tokens code@(c:ode) = 
            case c of
                '+'  -> help ((Token PLUS         "+"  start) : tokens) ode
                '-'  -> help ((Token MINUS        "-"  start) : tokens) ode
                '*'  -> help ((Token STAR         "*"  start) : tokens) ode
                '/'  -> help ((Token SLASH        "/"  start) : tokens) ode
                '\\' -> help ((Token RSLASH       "\\" start) : tokens) ode
                '!'  -> help ((Token EXPCLAMATION "!"  start) : tokens) ode
                '^'  -> help ((Token POWER        "^"  start) : tokens) ode
                '('  -> help ((Token LPAREN       "("  start) : tokens) ode
                ')'  -> help ((Token RPAREN       ")"  start) : tokens) ode
                '@'  -> help ((Token LAMBDA       "@"  start) : tokens) ode
                '\n' -> help ((Token SEMICOLON    "\n" start) : tokens) ode
                _ | c == '=' || c == '<' || c == '>' -> addEqual code
                _ | isNumber c -> addNum code False ""
                _ | isLetter c -> addName code
                _ | otherwise  -> error $ "Syntax error: " ++ (show c)
            where
                addEqual :: String -> [Token]
                addEqual ('<':'=':xs) = help ((Token LEQUAL    "<=" start) : tokens) xs
                addEqual ('>':'=':xs) = help ((Token GEQUAL    ">=" start) : tokens) xs
                addEqual ('=':'=':xs) = help ((Token DEQUAL    "==" start) : tokens) xs
                addEqual ('<':xs)     = help ((Token LESS      "<" start)  : tokens) ode
                addEqual ('>':xs)     = help ((Token GREAT     ">" start)  : tokens) ode
                addEqual ('=':xs)     = help ((Token EQUAL     "=" start)  : tokens) ode

                addNum :: String -> Bool -> String -> [Token]
                addNum ('.':xs) isFloat res = addNum xs True (res ++ ".") 
                addNum xs@(x:s) isFloat res | isNumber x = addNum s isFloat (res ++ [x])  
                                            | otherwise  = resultNum isFloat res xs
                addNum []       isFloat res = resultNum isFloat res []
                
                resultNum :: Bool -> String -> String -> [Token]
                resultNum True  res next = help ((Token FLOAT res start) : tokens) next
                resultNum False res next = help ((Token INT   res start) : tokens) next

                addName  slice = undefined

                start          = length code