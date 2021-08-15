module Lexer where

import Token
import Help

import Data.Char 
import Debug.Trace

scan :: String -> [Token]
scan []     = []
scan source = help [] source
    where
        help :: [Token] -> String -> [Token]
        help tokens ""            = reverse tokens
        help tokens (':':'=':ode) = help ((Token BIND ":=") : tokens) ode
        help tokens code@(c:ode)  = 
            case c of
                '+'  -> help ((Token PLUS         "+")  : tokens) ode
                '-'  -> help ((Token MINUS        "-")  : tokens) ode
                '*'  -> help ((Token STAR         "*")  : tokens) ode
                '/'  -> help ((Token SLASH        "/")  : tokens) ode
                '\\' -> help ((Token RSLASH       "\\") : tokens) ode
                '!'  -> help ((Token EXPCLAMATION "!")  : tokens) ode
                '^'  -> help ((Token POWER        "^")  : tokens) ode
                '('  -> help ((Token LPAREN       "(")  : tokens) ode
                ')'  -> help ((Token RPAREN       ")")  : tokens) ode
                '@'  -> help ((Token LAMBDA       "@")  : tokens) ode
                '\n' -> help ((Token SEMICOLON    "\n") : tokens) ode
                _ | c == '=' || c == '<' || c == '>' -> addEqual code
                _ | isNumber c -> addNum code False ""
                _ | isLetter c -> addName code
                _ | c == ' ' || c == '\t' || c == '\r' -> help tokens ode
                _ | otherwise  -> error $ "Syntax error: " ++ (show c)
            where
                addEqual :: String -> [Token]
                addEqual ('<':'=':xs) = help ((Token LEQUAL    "<=") : tokens) xs
                addEqual ('>':'=':xs) = help ((Token GEQUAL    ">=") : tokens) xs
                addEqual ('=':'=':xs) = help ((Token DEQUAL    "==") : tokens) xs
                addEqual ('<':xs)     = help ((Token LESS      "<")  : tokens) ode
                addEqual ('>':xs)     = help ((Token GREAT     ">")  : tokens) ode
                addEqual ('=':xs)     = help ((Token EQUAL     "=")  : tokens) ode

                addNum :: String -> Bool -> String -> [Token]
                addNum ('.':xs) isFloat res = addNum xs True (res ++ ".") 
                addNum xs@(x:s) isFloat res | isNumber x = addNum s isFloat (res ++ [x])  
                                            | otherwise  = resultNum isFloat res xs
                addNum []       isFloat res = resultNum isFloat res []
                
                resultNum :: Bool -> String -> String -> [Token]
                resultNum True  res next = help ((Token FLOAT res) : tokens) next
                resultNum False res next = help ((Token INT   res) : tokens) next

                addName code = help ((Token (fst3 token) (snd3 token)) : tokens) (thd3 token)
                    where
                        token = case code of
                            ('i':'f':xs)             -> (IF,    "if",    xs)
                            ('t':'h':'e':'n':xs)     -> (THEN,  "then",  xs)
                            ('e':'l':'s':'e':xs)     -> (ELSE,  "else",  xs)
                            ('t':'r':'u':'e':xs)     -> (TRUE,  "true",  xs)
                            ('f':'a':'l':'s':'e':xs) -> (FALSE, "false", xs)
                            ('a':'n':'d':xs)         -> (AND,   "and",   xs)
                            ('o':'r':xs)             -> (OR,    "or",    xs)
                            ('n':'o':'t':xs)         -> (NOT,   "not",   xs)
                            xs                       -> (NAME,  (takeWhile isLetter code), 
                                                                 dropWhile isLetter xs)
