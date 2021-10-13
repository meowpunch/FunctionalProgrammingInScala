package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap :
  lazy val genHeap: Gen[H] =
    frequency(
      (1, const(empty)),
      (10, for {
        v <- arbitrary[Int]
        h <- oneOf(const(empty), arbitrary[H])
      } yield insert(v, h))
    )

  given Arbitrary[H] = Arbitrary(genHeap)


  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("hint1: minOfTwo") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == math.min(a, b)
  }

  property("hint2: empty") = forAll { (a: Int) =>
    deleteMin(insert(a, empty)) == empty
  }

  def seqH(h: H): Seq[Int] =
    if (isEmpty(h)) return Nil
    else
      val x = findMin(h)
      Seq(x) ++ seqH(deleteMin(h))

  property("hint3: sortedSeq") = forAll { (h: H) =>

    val seq = seqH(h)
    seq.sorted(Ordering.Int) == seq
  }

  property("hint4: meld") = forAll { (h1: H, h2: H) =>
    seqH(meld(h1, h2)) == (seqH(h1) ++ seqH(h2)).sorted(Ordering.Int)
  }