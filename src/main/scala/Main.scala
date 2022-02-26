import scala.io.StdIn.readLine

object Main extends App {
  repl()

  def repl(): Unit = {
    print("< ")
    val input = readLine() + " "
    if (input == ":q" || input == ":quit") {
      System.exit(1)
    }
    val tokens = Lexer.scan(input)
    println(tokens)
    repl()
  }
}
