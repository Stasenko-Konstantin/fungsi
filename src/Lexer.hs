module Lexer where

import Token

scan :: String -> [Token]
scan []   = []
scan code = help [] $ (length code) - 1
	where
		help :: [Token] -> Int -> [Token]
		help tokens 0     = tokens
		help tokens start = tokens