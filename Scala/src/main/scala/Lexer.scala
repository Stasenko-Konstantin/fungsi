object Lexer {

  def scan(code: Option[String]): Option[List[Token]] = {

    def help(tokens: List[Token], start: Int): Option[List[Token]] = {

      def addNum(s: Int, r: String = "", isFloat: Boolean = false): Option[List[Token]] = {
        def result(): Option[List[Token]] = help(Token(if isFloat then Token.FLOAT
                                            else Token.INT, r.reverse) :: tokens, s)
        if s < 0 then return result()
        val c = code.get(s)
        c match {
          case '.' => addNum(s - 1, r + c, true)
          case _ => if !c.isDigit then result() else addNum(s - 1, r + c, isFloat)
        }
      }

      def addName(s: Int, r: String = ""): Option[List[Token]] = {
        def result(): Option[List[Token]] = {
          var t: TokenType = Token.NIL
          r.reverse match {
            case "if" => t = Token.IF
            case "then" => t = Token.THEN
            case "else" => t = Token.ELSE
            case "true" => t = Token.TRUE
            case "false" => t = Token.FALSE
            case "and" => t = Token.AND
            case "or" => t = Token.OR
            case "not" => t = Token.NOT
            case _ => t = Token.NAME
          }
          help(Token(t, r.reverse) :: tokens, s)
        }
        if s < 0 then return result()
        val c = code.get(s)
        if c.isLetter then addName(s - 1, r + c) else result()
      }

      def addEqual(s: Int): Option[List[Token]] = {
        def result(t: TokenType, r: String, offset: Int = 1): Option[List[Token]] =
          help(Token(t, r) :: tokens, s - offset)
        val c = code.get(s - 1)
        if (c.toString + code.get(s).toString) == ":=" then result(Token.BIND, ":=", 2)
        else c match {
          case '<' => result(Token.LEQUAL, "<=", 2)
          case '>' => result(Token.GEQUAL, ">=", 2)
          case '=' => result(Token.DEQUAL, "==", 2)
          case _ => code.get(s) match {
            case '=' => result(Token.EQUAL, "=")
            case '<' => result(Token.LESS, "<")
            case '>' => result(Token.GREAT, ">")
          }
        }
      }

      if start < 0 then return Option(tokens)
      val c = code.get(start)
      code match {
        case Some(code) => c match {
          case '+' => help(Token(Token.PLUS, "+") :: tokens, start - 1)
          case '-' => help(Token(Token.MINUS, "-") :: tokens, start - 1)
          case '*' => help(Token(Token.STAR, "*") :: tokens, start - 1)
          case '/' => help(Token(Token.SLASH, "/") :: tokens, start - 1)
          case '\\' => help(Token(Token.RSLASH, "\\") :: tokens, start - 1)
          case '!' => help(Token(Token.EXPCLAMATION, "!") :: tokens, start - 1)
          case '^' => help(Token(Token.POWER, "^") :: tokens, start - 1)
          case '(' => help(Token(Token.LPAREN, "(") :: tokens, start - 1)
          case ')' => help(Token(Token.RPAREN, ")") :: tokens, start - 1)
          case '@' => help(Token(Token.LAMBDA, "@") :: tokens, start - 1)
          case '=' | '<' | '>' | ':' => addEqual(start)
          case '\n' => help(Token(Token.SEMICOLON, "\\n") :: tokens, start - 1)
          case ' ' | '\t' | '\r' => help(tokens, start - 1)
          case _ if c.isDigit => addNum(start)
          case _ if c.isLetter => addName(start)
          case _ => Main.error("Syntax error: ", c.toString)
                    Option(tokens)
        }
        case None => None
      }
    }

    val len = code.get.length - 1
    Option(help(Nil, len).get ::: List(Token(Token.EOF, "\u0000")))
  }
}