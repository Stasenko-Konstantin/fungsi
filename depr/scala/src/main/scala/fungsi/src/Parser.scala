package fungsi.src

sealed abstract class AST

object AST:
  case class Term(term: Token.Term)
  case class Expr(nonterm: Token.Nonterm, ast: AST)

object Parser:
  def parse(tokens: List[Token]): AST = null
end Parser


