package fungsi.src

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main extends App :
  repl()

  @tailrec
  def repl(): Unit =
    print("< ")
    val input = readLine() + "\n"
    if input == ":q" || input == ":quit" then
      System.exit(1)
    val tokens = Lexer.scan(input)
    println(tokens)
    val ASTs = Parser.parse(tokens)
    println(ASTs)
    repl()

