module Parser where

import Token
import Help

parse :: [Token] -> Expr
parse tokens@(t:okens) = start tokens
	where
		start (Token token content subtype) = undefined