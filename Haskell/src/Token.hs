module Token where

data TokenType = PLUS      | MINUS  | STAR   | SLASH | RSLASH | EXPCLAMATION | POWER  |
                 BIND      | EQUAL  | DEQUAL | LESS  | GREAT  | LEQUAL       | GEQUAL |
                 FALSE     | TRUE   | IF     | THEN  | ELSE   | AND          | OR     | NOT |
                 NAME      | LAMBDA | 
                 INT       | FLOAT  |
                 LPAREN    | RPAREN |
                 SEMICOLON | EOF    | 
                 NIL                  deriving Show

data Token = Token { token :: TokenType, content :: String }

instance Show Token where
    show (Token token content) = 
        " {token = " ++ (show token) ++ 
        ", content = " ++ (show content) ++ "} "