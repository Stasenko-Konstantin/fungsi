package fungsi.src

sealed abstract class TokenType

class Token(val token: TokenType, val content: String):
  override def toString: String = "{ " + token + ", " + content + " }"
  
  def toList: List[Token] = List(this)

object Token:
  case object DEF extends TokenType // def
  case object BIND extends TokenType // :=
  case object SEMICOLON extends TokenType // ;
  case object COMMA extends TokenType // ,
  case object NAME extends TokenType
  case object ATOM extends TokenType // :{name}
  case object LAMBDA extends TokenType // @
  case object DELIMITER extends TokenType // |
  case object QUOTE extends TokenType // ~
  case object RETURN extends TokenType // ^
  case object NUM extends TokenType
  case object CHAR extends TokenType
  case object STRING extends TokenType
  case object LPAREN extends TokenType // ( or [
  case object RPAREN extends TokenType // ) or ]
  case object EOF extends TokenType
