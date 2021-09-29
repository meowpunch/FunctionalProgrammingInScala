trait Expr

case class NumbeR(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Var(name: String) extends Expr

case class Product(e1: Expr, e2: Expr) extends Expr

class Ration(val numer: Int, val denum: Int) extends Expr


def eval(e: Expr): Int = e match
  case NumbeR(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)


def show(e: Expr): String =
  def showP(e: Expr): String = e match
    case e: Sum => s"(${show(e)})"
    case _ => show(e)

  e match {
    case NumbeR(n) => n.toString
    case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
    case Var(x) => x
    case Product(e1, e2) => s"${showP(e1)} * ${showP(e2)}"
    case e: Ration => s"${e.numer} / ${e.denum}"
  }


val expr = Sum(NumbeR(1), NumbeR(1))
eval(expr)
show(expr)
val expr1 = Product(expr, Var("x"))
show(expr1)
show(Ration(1, 2))

case class User(name: String, age: Int)

val u1 = User("meow", 7)
val u2 = User.apply("meow", 7)
u1 == u2
val tuple = ("meow", 7)
val u3 = (User.apply _).tupled(tuple)
u2 == u3


