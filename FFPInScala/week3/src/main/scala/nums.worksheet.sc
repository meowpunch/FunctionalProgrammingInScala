// not using primitive int
abstract class Nat:
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat

  def +(that: Nat): Nat

  def -(that: Nat): Nat
end Nat

class Succ(n: Nat) extends Nat :
  def isZero: Boolean = false

  def predecessor: Nat = n

  def successor: Nat = Succ(this)

  def +(that: Nat): Nat = this.predecessor + that.successor

  def -(that: Nat): Nat =
    if that.isZero then this
    else this.predecessor - that.predecessor

  override def toString: String = s"Succ($n)"

object Zero extends Nat :
  def isZero: Boolean = true

  def predecessor: Nat = throw new NoSuchElementException("Zero.predecessor")

  def successor: Nat = Succ(this)

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat =
    if that.isZero then this
    else throw new NoSuchElementException("Nat should be positive")

  override def toString: String = "Zero"

val two = Succ(Succ(Zero))
val one = Succ(Zero)

one + two
two - one
one - two