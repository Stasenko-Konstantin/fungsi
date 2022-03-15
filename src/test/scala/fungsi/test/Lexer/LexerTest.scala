package fungsi.test.Lexer

import fungsi.src.Lexer
import fungsi.src.Token

trait LexerTest extends munit.Assertions:
  extension (src: String)
    infix def shouldScan(toks: List[Token]): Unit =
      val scan = Lexer.scan(src)
      assert(scan.exists(a => toks.exists(b => a equals b)))