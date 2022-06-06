package fungsi.test.Lexer

import fungsi.src.Token

class TokenTests extends munit.FunSuite with LexerTest:
  test("num") {
    "1.2 " shouldScan Token(Token.NUM, "1.2").toList
  }

  test("lambda") {
    "@[a b | ^ a + b] " shouldScan List(
      Token(Token.LAMBDA, "@"),
      Token(Token.LPAREN, "("),
      Token(Token.NAME, "a"),
      Token(Token.NAME, "b"),
      Token(Token.DELIMITER, "|"),
      Token(Token.RETURN, "^"),
      Token(Token.NAME, "a"),
      Token(Token.NAME, "+"),
      Token(Token.NAME, "b"),
      Token(Token.RPAREN, ")")
    )
  }
end TokenTests

