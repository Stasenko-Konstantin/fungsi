package fungsi.test.Lexer

import fungsi.src.Lexer
import fungsi.src.Token

trait LexerTest extends munit.Assertions:
  extension (src: String)
    infix def shouldScan(toks: List[Token]): Unit =
      assert(Lexer.scan(src) == toks)