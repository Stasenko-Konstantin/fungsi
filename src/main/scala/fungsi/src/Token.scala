package fungsi.src

sealed abstract class TokenType

class Token(val token: TokenType, val content: String):
  override def toString: String = "{ " + token + ", " + content + " }"
  
  def toList: List[Token] = List(this)

  def equals(that: Token): Boolean = this.content == that.content

object Token:
  trait Term
  trait Nonterm

  case object DEF extends TokenType with Term // def
  case object BIND extends TokenType with Nonterm // :=
  case object SEMICOLON extends TokenType with Nonterm // ;
  case object COMMA extends TokenType with Nonterm // ,
  case object NAME extends TokenType with Term
  case object ATOM extends TokenType with Term // :{name}
  case object LAMBDA extends TokenType with Term// @
  case object DELIMITER extends TokenType with Nonterm // |
  case object QUOTE extends TokenType with Nonterm // $
  case object RETURN extends TokenType with Nonterm // ^
  case object NUM extends TokenType with Term
  case object CHAR extends TokenType with Term
  case object STRING extends TokenType with Term
  case object LPAREN extends TokenType with Term// ( or [
  case object RPAREN extends TokenType with Nonterm // ) or ]
  case object EOF extends TokenType with Nonterm
