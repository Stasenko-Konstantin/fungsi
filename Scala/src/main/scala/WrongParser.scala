class WrongParser(tokens: List[Token]) {
  
  var curr = 0;

  def expression(): Expr = equality()

  def equality(): Expr =
    var expr = comparison()

    while (matchExpr(Token.EQUAL, Token.DEQUAL)) {
      val left = previous()
      val right = comparison()
    }

    expr

  def matchExpr(types: TokenType*): Boolean =
    for (mytype <- types) {
      if (check(mytype)) {
        advance()
        return true
      }
    }
    false // Надеюсь до сюда не дойдет

  def check(token: TokenType): Boolean =
    if (isAtEnd()) false
    peek().token == token

  def advance(): Token =
    if (!isAtEnd()) curr += 1
    previous()

  def isAtEnd(): Boolean = peek().token == Token.EOF

  def peek(): Token = tokens(curr)

  def previous(): Token = tokens(curr - 1)

  def comparison(): Expr =
    var left = term()

    while(matchExpr(Token.GREAT, Token.GEQUAL, Token.LESS, Token.LEQUAL)) {
      val middle = previous()
      val right = term()
      left = Infix(left, middle, right)
    }

    left

  def term(): Expr =
    var left = factor()

    while(matchExpr(Token.MINUS, Token.PLUS)) {
      val middle = previous()
      val right = factor()
      left = Infix(left, middle, right)
    }

    left

  def factor(): Expr =
    var left = unary()

    while(matchExpr(Token.SLASH, Token.STAR)) {
      val middle = previous()
      val right = unary()
      left = Infix(left, middle, right)
    }

    left

  def unary(): Expr =
    if (matchExpr(Token.MINUS)) {
      val left = previous()
      val right = unary()
      Prefix(left, right)
    }
    Main.error("Expect expression", "")
    Literal(null) //До сюда не должно дойти

  def primary(): Expr =
    if (matchExpr(Token.FALSE)) Literal(false)
    if (matchExpr(Token.TRUE)) Literal(true)
    if (matchExpr(Token.NIL)) Literal(null)
    if (matchExpr(Token.INT, Token.FLOAT, Token.NAME)) Literal(previous().content)

    if (matchExpr(Token.LPAREN)) {
      val expr = expression()
      consume(Token.RPAREN, "Expect ')'")
      Group(expr)
    }

    Main.error("Expect expressiom", "")
    primary() // До сюда не должно дойти

  def consume(token: TokenType, msg: String): Token =
    if (check(token)) advance()
    Main.error(msg, "")
    advance() // Поток управления не дойдет до сюда, а типы разрешить надо

  def synchronize(): Unit =
    advance()

    while (!isAtEnd()) {
      if (previous().token == Token.SEMICOLON) return

      peek().token match {
        case Token.IF | Token.THEN | Token.ELSE | Token.NOT | Token.OR | Token.AND =>
          return
      }

      advance()
    }

  def parse(): Expr = expression()
}
