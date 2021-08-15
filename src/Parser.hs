module Parser where

import Token
import Expr
import Help

parse :: [Token] -> Expr
parse tokens@(t:okens) = help tokens 1
	where
		help :: [Token] -> Int -> Expr
		help ((Token MINUS  _) : xs) curr = undefined
		help ((Token IF     _) : xs) curr = undefined
		help ((Token NOT    _) : xs) curr = Prefix (Token NOT "not") (help xs (curr + 1))
		help ((Token LAMBDA _) : xs) curr = undefined
		help ((Token LPAREN _) : xs) curr = undefined

		help ((Token NAME  content) : xs) curr = undefined
		help ((Token INT   content) : xs) curr = undefined
		help ((Token FLOAT content) : xs) curr = undefined
		help ((Token FALSE _) : xs) curr = undefined
		help ((Token TRUE  _) : xs) curr = undefined

		help [(Token NAME  content)] _ = Literal (Token NAME  content)
		help [(Token INT   content)] _ = Literal (Token INT   content)
		help [(Token FLOAT content)] _ = Literal (Token FLOAT content)
		help [(Token FALSE _)] _ = Literal (Token FALSE "false")
		help [(Token TRUE  _)] _ = Literal (Token TRUE  "true")

		help _ _ = error "Semantic error"

		end = length tokens