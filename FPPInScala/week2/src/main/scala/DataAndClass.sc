class Rational(x: Int, y: Int): // val x, val y
  require(y > 0, "denominator must be positive")
  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a%b)

  // if numer and denom are called infrequently, `def` is better than `val`
  val numer = x / gcd(x, y)
  val denom = y / gcd(x, y)

  // a/b + c/d = (a*d + c*b)/b*d
  def add(that: Rational): Rational = Rational(this.numer * that.denom + that.numer * this.denom, this.denom * that.denom)
  def + (that: Rational): Rational = this.add(that)

  // a/b < c/d => a*d < b*c
  def less(that: Rational): Boolean = this.numer * that.denom < that.numer * this.denom

  override def toString: String = s"$numer/$denom"


extension (r: Rational)
  def min(s: Rational): Rational = if r.less(s) then r else s

Rational(1, 0)  // IllegalArgumentException

val r = Rational(2, 4)
val s = Rational(1, 3)

r.less(s)

r.add(s)
r add s

r + s
r.+(s)

r.min(s)
r min s
