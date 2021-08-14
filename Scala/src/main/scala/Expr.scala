abstract class Expr

case class Infix(left: Expr, middle: Token, right: Expr) extends Expr

case class Postfix(left: Expr, right: Token) extends Expr

case class Prefix(left: Token, right: Expr) extends Expr

case class Grouping(expr: Expr) extends Expr

case class Literal(value: Any) extends Expr

