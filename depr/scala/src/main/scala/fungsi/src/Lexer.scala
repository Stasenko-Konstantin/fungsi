package fungsi.src

import scala.annotation.tailrec

object Lexer:
  def scan(code: String): List[Token] =
    val symbols = "+-\\|/?.><!#@`^~%&*-_+="
    val keywords = Map("def" -> Token.DEF)

    @tailrec
    def HELP(tokens: List[Token], code: String): List[Token] =
      if code == "" then
        return (new Token(Token.EOF, "") +: tokens).reverse
      val c = code.head
      c match
        case '@' => HELP(new Token(Token.LAMBDA, "@") +: tokens, code.tail)
        case '$' => HELP(new Token(Token.QUOTE, "~") +: tokens, code.tail)
        case '^' => HELP(new Token(Token.RETURN, "^") +: tokens, code.tail)
        case '|' => HELP(new Token(Token.DELIMITER, "|") +: tokens, code.tail)
        case ';' => HELP(new Token(Token.SEMICOLON, ";") +: tokens, scipSpaces(code.tail))
        case ',' => HELP(new Token(Token.COMMA, ",") +: tokens, code.tail)
        case ':' =>
          if code.tail.head == '=' then
            HELP(new Token(Token.BIND, ":=") +: tokens, code.tail.tail)
          else
            val name = addName(code.tail, "")
            HELP(new Token(Token.ATOM, ":" + name._1) +: tokens, name._2)
        case '(' | '[' => HELP(new Token(Token.LPAREN, "(") +: tokens, code.tail)
        case ')' | ']' => HELP(new Token(Token.RPAREN, ")") +: tokens, code.tail)
        case '\r' | '\t' | '\n' | ' ' => HELP(tokens, code.tail)
        case _ if c.isDigit =>
          val num = addNum(code, "")
          HELP(new Token(Token.NUM, num._1) +: tokens, num._2)
        case _ if c.isLetter || symbols.contains(c) =>
          val name = addName(code, "")
          keywords get name._1 match {
            case Some(tok) if tok == Token.DEF =>
              HELP(new Token(keywords(name._1), name._1) +: tokens, name._2)
            case None | Some(_) =>
              HELP(new Token(Token.NAME, name._1) +: tokens, name._2)
          }
        case _ =>
          println("lexer error")
          Nil
    end HELP

    @tailrec
    def addSeq(code: String, res: String, end: Char): (String, String) =
      if code.head == end then
        return (res.reverse, code.tail)
      addSeq(code.tail, code.head +: res, end)
    end addSeq

    @tailrec
    def addNum(code: String, res: String): (String, String) =
      def isDigit(c: Char): Boolean = c.isDigit || c == '.'

      if !isDigit(code.head) then
        return (res.reverse, code)
      addNum(code.tail, code.head +: res)
    end addNum

    @tailrec
    def addName(code: String, res: String): (String, String) =
      def isLetter(c: Char): Boolean = c.isLetter || c.isDigit || symbols.contains(c)

      if !isLetter(code.head) then
        return (res.reverse.toLowerCase, code)
      addName(code.tail, code.head +: res)
    end addName

    @tailrec
    def scipSpaces(code: String): String =
      if code.head == '\n' then
        code.tail
      else
        scipSpaces(code.tail)

    HELP(Nil, code)
  end scan
end Lexer