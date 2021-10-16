
trait Expr {
  def eval: Int
}


class Number(n: Int) extends Expr {
  override def eval: Int = n
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  override def eval: Int = e1.eval + e2.eval
}

// It is easy to add data than operation