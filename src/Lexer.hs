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
        help tokens (':':'=':ode) = help ((Token BIND ":=" "None") : tokens) ode
        help tokens code@(c:ode)  = 
            case c of
                '+'  -> help ((simpleToken PLUS         "+")  : tokens) ode
                '-'  -> help ((simpleToken MINUS        "-")  : tokens) ode
                '*'  -> help ((simpleToken STAR         "*")  : tokens) ode
                '/'  -> help ((simpleToken SLASH        "/")  : tokens) ode
                '\\' -> help ((simpleToken RSLASH       "\\") : tokens) ode
                '!'  -> help ((simpleToken EXPCLAMATION "!")  : tokens) ode
                '^'  -> help ((simpleToken POWER        "^")  : tokens) ode
                '@'  -> help ((simpleToken LAMBDA       "@")  : tokens) ode
                '\n' -> help ((simpleToken SEMICOLON    "\n") : tokens) ode
                _ | c == '(' || c == '[' -> help ((simpleToken LPAREN "(") : tokens) ode
                _ | c == ')' || c == ']' -> help ((simpleToken RPAREN ")") : tokens) ode
                _ | c == '=' || c == '<' || c == '>' -> addEqual code
                _ | isNumber c -> addNum code False ""
                _ | isLetter c -> addName code
                _ | c == ' ' || c == '\t' || c == '\r' -> help tokens ode
                _ | otherwise  -> error $ "Syntax error: " ++ (show c)
            where
                simpleToken :: TokenType -> String -> Token
                simpleToken token content = Token token content "None"

                addEqual :: String -> [Token]
                addEqual ('<':'=':xs) = help ((simpleToken LEQUAL    "<=") : tokens) xs
                addEqual ('>':'=':xs) = help ((simpleToken GEQUAL    ">=") : tokens) xs
                addEqual ('=':'=':xs) = help ((simpleToken DEQUAL    "==") : tokens) xs
                addEqual ('<':xs)     = help ((simpleToken LESS      "<")  : tokens) ode
                addEqual ('>':xs)     = help ((simpleToken GREAT     ">")  : tokens) ode
                addEqual ('=':xs)     = help ((simpleToken EQUAL     "=")  : tokens) ode

                addNum :: String -> Bool -> String -> [Token]
                addNum ('.':xs) isFloat res = addNum xs True (res ++ ".") 
                addNum xs@(x:s) isFloat res | isNumber x = addNum s isFloat (res ++ [x])  
                                            | otherwise  = resultNum isFloat res xs
                addNum []       isFloat res = resultNum isFloat res []
                
                resultNum :: Bool -> String -> String -> [Token]
                resultNum True  res next = help ((Token FLOAT res "Num") : tokens) next
                resultNum False res next = help ((Token INT   res "Num") : tokens) next

                addName code = help ((Token (fst4 token) (snd4 token) (thd4 token)) : tokens) (fth4 token)
                    where
                        token = case code of
                            ('i':'f':xs)             -> (IF,    "if",    "Name", xs)
                            ('t':'h':'e':'n':xs)     -> (THEN,  "then",  "None", xs)
                            ('e':'l':'s':'e':xs)     -> (ELSE,  "else",  "None", xs)
                            ('t':'r':'u':'e':xs)     -> (TRUE,  "true",  "Name", xs)
                            ('f':'a':'l':'s':'e':xs) -> (FALSE, "false", "Name", xs)
                            ('a':'n':'d':xs)         -> (AND,   "and",   "Name", xs)
                            ('o':'r':xs)             -> (OR,    "or",    "Name", xs)
                            ('n':'o':'t':xs)         -> (NOT,   "not",   "Name", xs)
                            ('n':'i':'l':xs)         -> (NIL,   "nil",   "Name", xs)
                            xs                       -> (NAME,  (takeWhile p code), "Name", 
                                                                (dropWhile p xs))
                                where
                                    p c = isLetter c || isNumber c
