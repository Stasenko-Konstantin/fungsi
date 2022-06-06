module Token where

data TokenType
  = PLUS
  | MINUS
  | STAR
  | SLASH
  | BIND
  | EQUAL
  | DEQUAL
  | LESS
  | GREAT
  | LEQUAL
  | GEQUAL
  | NEQUAL
  | FALSE
  | TRUE
  | IF
  | THEN
  | ELSE
  | AND
  | OR
  | NOT
  | NAME
  | LAMBDA
  | INT
  | FLOAT
  | RPAREN
  | LPAREN
  | RCPAREN
  | LCPAREN
  | COMMA
  | VBAR
  | SEMICOLON
  | EOF
  | NIL
  deriving (Show)

data Category = None | Num | Name | InfixOp

data Token = Token {token :: TokenType, content :: String, suptype :: Category}

instance Eq Token where
  (==) (Token _ c1 _) (Token _ c2 _) = c1 == c2

instance Show Token where
  show (Token token content _) =
    " {token = " ++ show token
      ++ ", content = "
      ++ show content
      ++ "} "