
trait Expr

case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Var(v: String) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

def show(e: Expr): String = {
  def sumBraces(e: Expr): String = {
    e match {
      case Sum(e1, e2) => s"(${show(e1)} + ${show(e2)})"
      case _ => show(e)
    }
  }

  e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
    case Var(v) => v
    case Prod(e1, e2) => s"${sumBraces(e1)} * ${sumBraces(e2)}"
  }
}

show(Sum(Sum(Number(3), Number(1)), Number(2)))
show(Sum(Prod(Number(2), Var("x")), Var("y")))
show(Prod(Sum(Number(2), Var("x")), Var("y")))