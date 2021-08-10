module Lexer where

import Token

scan :: String -> [Token]
scan []     = []
scan source = help [] source
	where
		help :: [Token] -> String -> [Token]
		help tokens ""           = reverse tokens
		help tokens code@(c:ode) = 
			case c of
				'+'  -> help ((Token.Token Token.PLUS         "+"  start) : tokens) ode
				'-'  -> help ((Token.Token Token.MINUS        "-"  start) : tokens) ode
				'*'  -> help ((Token.Token Token.STAR         "*"  start) : tokens) ode
				'/'  -> help ((Token.Token Token.SLASH        "/"  start) : tokens) ode
				'\\' -> help ((Token.Token Token.RSLASH       "\\" start) : tokens) ode
				'!'  -> help ((Token.Token Token.EXPCLAMATION "!"  start) : tokens) ode
				'^'  -> help ((Token.Token Token.POWER        "^"  start) : tokens) ode
				'('  -> help ((Token.Token Token.LPAREN       "("  start) : tokens) ode
				')'  -> help ((Token.Token Token.RPAREN       ")"  start) : tokens) ode
				'@'  -> help ((Token.Token Token.LAMBDA       "@"  start) : tokens) ode
				'\n' -> help ((Token.Token Token.SEMICOLON    "\n" start) : tokens) ode
				_ | c == '=' || c == '<' || c == '>' -> addEqual code
				_ | isDigit  c -> addNum code
				_ | isLetter c -> addName code
				_ | otherwise  -> error $ "Syntax error: " ++ (show c)
			where
				addEqual pos  = undefined 
				isDigit  char = undefined
				isLetter char = undefined
				addNum   pos  = undefined
				addName  pos  = undefined
				start         = length code