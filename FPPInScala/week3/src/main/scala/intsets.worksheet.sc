abstract class IntSet:
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(s: IntSet): IntSet

  def union2(s: IntSet): IntSet

  def union3(s: IntSet): IntSet

/**
 * store Int to tree (sorted like binarytree)
 *
 * @param elem
 * @param left
 * @param right
 */
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet :
  def incl(x: Int): IntSet =
    if x < elem then NonEmpty(elem, left.incl(x), right)
    else if x > elem then NonEmpty(elem, left, right.incl(x))
    else this

  def contains(x: Int): Boolean =
    if x < elem then left.contains(x)
    else if x > elem then right.contains(x)
    else true

  /**
   * this: {1, 2, 4, 5}
   *
   * @param s {2, 3, 6, 7}
   * @return {1, 2, 3, 4, 5, 6, 7}
   */
  def union(s: IntSet): IntSet =
    println(s"$this union $s")
    left.union(right).union(s).incl(elem)

  def union2(s: IntSet): IntSet =
    println(s"$this union2 $s")
    left.union2(right.union2(s)).incl(elem)

  def union3(s: IntSet): IntSet =
    println(s"$this union3 $s")
    left.union3(s.union3(right)).incl(elem)

  // print inorder
  override def toString: String =
    s"$left $elem $right"


object Empty extends IntSet :
  def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)

  def contains(x: Int): Boolean = false

  def union(s: IntSet): IntSet = s

  def union2(s: IntSet): IntSet = s

  def union3(s: IntSet): IntSet = s

  override def toString: String = ""

object IntSet:
  def apply(): IntSet = Empty

  def apply(e: Int) = Empty.incl(e)

  def apply(a: Int, b: Int) = Empty.incl(a).incl(b)


NonEmpty(1, Empty, Empty).incl(2).incl(4).incl(5)
val s1 = NonEmpty(4, Empty, Empty).incl(1).incl(2).incl(5).incl(9)
val s2 = NonEmpty(3, Empty, Empty).incl(5).incl(2).incl(7)
s1.union(s2)
s1.union2(s2)
s1.union3(s2)