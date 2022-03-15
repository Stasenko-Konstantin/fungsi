package fungsi.src

import scala.annotation.tailrec

object Lexer:
  def scan(code: String): List[Token] =
    val symbols = "+-\\|/?.><!#@`^~%&*-_+="
    val keywords = Map("def" -> Token.DEF)
  
    @tailrec
    def help(tokens: List[Token], code: String): List[Token] =
      if code == "" then
        return (new Token(Token.EOF, "") +: tokens).reverse
      val c = code.head
      c match
        case '@' => help(new Token(Token.LAMBDA, "@") +: tokens, code.tail)
        case '$' => help(new Token(Token.QUOTE, "~") +: tokens, code.tail)
        case '^' => help(new Token(Token.RETURN, "^") +: tokens, code.tail)
        case '|' => help(new Token(Token.DELIMITER, "|") +: tokens, code.tail)
        case ';' => help(new Token(Token.SEMICOLON, ";") +: tokens, code.tail)
        case ',' => help(new Token(Token.COMMA, ",") +: tokens, code.tail)
        case ':' =>
          if code.tail.head == '=' then
            help(new Token(Token.BIND, ":=") +: tokens, code.tail.tail)
          else
            val name = addName(code.tail, "")
            help(new Token(Token.NAME, ":" + name._1) +: tokens, name._2)
        case '(' | '[' => help(new Token(Token.LPAREN, "(") +: tokens, code.tail)
        case ')' | ']' => help(new Token(Token.RPAREN, ")") +: tokens, code.tail)
        case '\r' | '\t' | ' ' => help(tokens, code.tail)
        case '\'' =>
          val seq = addSeq(code.tail, "", '\'')
          help(new Token(Token.CHAR, seq._1) +: tokens, seq._2)
        case '"' =>
          val seq = addSeq(code.tail, "", '"')
          help(new Token(Token.STRING, seq._1) +: tokens, seq._2)
        case _ if c.isDigit =>
          val num = addNum(code, "")
          help(new Token(Token.NUM, num._1) +: tokens, num._2)
        case _ if c.isLetter || symbols.contains(c) =>
          val name = addName(code, "")
          keywords get name._1 match {
            case Some(tok) if tok == Token.DEF =>
              help(new Token(keywords(name._1), name._1) +: tokens, name._2)
            case None | Some(_) =>
              help(new Token(Token.NAME, name._1) +: tokens, name._2)
          }
        case _ =>
          println("lexer error")
          Nil
    end help
  
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
    
    help(Nil, code)
  end scan
end Lexer
