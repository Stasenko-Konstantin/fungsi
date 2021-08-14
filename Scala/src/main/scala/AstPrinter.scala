import scala.annotation.tailrec

object AstPrinter:
  def print(expr: Expr): String =
    expr match {
      case in: Infix => parenthesize(in.middle.content, in.left :: in.right :: Nil)
      case pre: Prefix => parenthesize(pre.left.content, pre.right :: Nil)
      case post: Postfix => parenthesize(post.right.content, post.left :: Nil)
      case group: Grouping => parenthesize("group", group.expr :: Nil)
      case liter: Literal => liter.value match {
        case Some(l) => l.toString
        case None => "nil"
      }
    }

  def parenthesize(name: String, exprs: List[Expr]): String =

    def help(acc: String, leftovers: List[Expr]): String =
      leftovers match {
        case Nil => acc
        case _   => help(acc + " " + print(leftovers.head), leftovers.tail)
      }
    help("(" + name, exprs) + ")"