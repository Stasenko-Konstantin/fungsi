package fungsi.test.Lexer

import fungsi.src.Token

class TokenTests extends munit.FunSuite with LexerTest:
  private val eof = Token(Token.EOF, "")
  test("num") {
    "1.2" shouldScan Token(Token.NUM, "1.2").toList :+ eof
  }
end TokenTests

