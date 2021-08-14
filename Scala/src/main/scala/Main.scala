//
// repl # условие
//

import scala.io.StdIn.readLine

object Main:

  def main(args: Array[String]): Unit =
    print("< ")
    val input = Option(" " + readLine().toLowerCase)
    val tokens = Lexer.scan(input)
    tokens.get.foreach(println)
    //val expr = Parser.parse(tokens.get)
    //println(AstPrinter.print(expr))
    if input.get == ":q" then main(args) // == -> !=

  def error(msg: String, content: String): Unit =
    val out = msg + content
    System.err.println(out)