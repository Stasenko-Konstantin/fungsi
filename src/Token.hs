module Token where

data TokenType = PLUS      | MINUS  | STAR   | SLASH | RSLASH | EXPCLAMATION | POWER  |
				 EQUAL     | DEQUAL | TEQUAL | LESS  | GREAT  | LEQUAL       | GEQUAL |
				 FALSE     | TRUE   | IF     | THEN  | ELSE   | AND          | OR     | NOT |
				 NAME      | LAMBDA | SQRT   | 
				 INT       | FLOAT  |
				 LPAREN    | RPAREN |
				 SEMICOLON | EOF    | 
				 NIL                  deriving Show

data Token = Token { token :: TokenType, content :: String, pos :: Int }

instance Show Token where
	show (Token token content pos) = 
		" {token = " ++ (show token) ++ 
		", content = " ++ (show content) ++ 
		", pos = " ++ (show pos) ++ "} "