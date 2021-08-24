module Parser where

import Token
import Help

parse :: [Token] -> Expr
parse tokens@(t:okens) = case t of
	{_, _, "Num"}  -> parseNum
	{_, _, "Name"} -> parseName
	{_, "(", _}    -> parseGroup
	{_, "@", _}    -> parseLambda
	{_, "-", _}    -> parseUnaryMinus
	{_, _, "None"} -> error "Parse error"
	where
		parseNum        = undefined
		parseName       = undefined
		parseGroup      = undefined
		parseLambda     = undefined
		parseUnaryMinus = undefined