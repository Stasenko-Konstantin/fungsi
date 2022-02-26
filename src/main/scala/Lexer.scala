import scala.annotation.tailrec

object Lexer {
  def scan(code: String): List[Token] = {
    val symbols = "+-:\\|/?.>,<!#@`^~%&*-_+="

    @tailrec
    def help(tokens: List[Token], code: String): List[Token] = {
      if (code == "") {
        return (new Token(Token.EOF, "") +: tokens).reverse
      }
      val c = code.head
      c match {
        case '@' => help(new Token(Token.LAMBDA, "@") +: tokens, code.tail)
        case '~' => help(new Token(Token.QUOTE, "~") +: tokens, code.tail)
        case '^' => help(new Token(Token.RETURN, "^") +: tokens, code.tail)
        case '|' => help(new Token(Token.DELIMITER, "|") +: tokens, code.tail)
        case '(' | '[' => help(new Token(Token.LPAREN, "(") +: tokens, code.tail)
        case ')' | ']' => help(new Token(Token.RPAREN, ")") +: tokens, code.tail)
        case '\n' => help(new Token(Token.SEMICOLON, "\\n") +: tokens, code.tail)
        case '\r' | '\t' | ' ' => help(tokens, code.tail)
        case '\'' =>
          val seq = addSeq(code.tail, "", '\'')
          help(new Token(Token.CHAR, seq._1.toString) +: tokens, seq._2)
        case '"' =>
          val seq = addSeq(code.tail, "", '"')
          help(new Token(Token.STRING, seq._1.toString) +: tokens, seq._2)
        case _ if c.isDigit =>
          val num = addNum(code, "")
          help(new Token(Token.NUM, num._1) +: tokens, num._2)
        case _ if c.isLetter || symbols.contains(c) =>
          val name = addName(code, "")
          help(new Token(Token.NAME, name._1) +: tokens, name._2)
        case _ =>
          println("lexer error")
          Nil
      }
    }

    @tailrec
    def addSeq(code: String, res: String, end: Char): (String, String) = {
      if (code.head == end) {
        return (res.reverse, code.tail)
      }
      addSeq(code.tail, code.head +: res, end)
    }

    @tailrec
    def addNum(code: String, res: String): (String, String) = {
      def isDigit(c: Char): Boolean = c.isDigit || c == '.'

      if (!isDigit(code.head)) {
        return (res.reverse, code)
      }
      addNum(code.tail, code.head +: res)
    }

    @tailrec
    def addName(code: String, res: String): (String, String) = {
      def isLetter(c: Char): Boolean = c.isLetter || symbols.contains(c)

      if (!isLetter(code.head)) {
        return (res.reverse.toLowerCase, code)
      }
      addName(code.tail, code.head +: res)
    }
    help(Nil, code)
  }
}
