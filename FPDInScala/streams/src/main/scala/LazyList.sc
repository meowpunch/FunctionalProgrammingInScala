

val ll: LazyList[Int] = LazyList.from(2).filter(
  x => {
    println(x)
    val cand = ll.takeWhile(_ < x) // cannot terminate in this point
    print(cand.toList)
    cand.forall(_ + 1 < x)
  }
)


ll.take(2).toList // doesn't work

def makeLazyList(s: LazyList[Int]): LazyList[Int] =
  // don't need a base case
  // the rest of #:: will be not evaluated
  s.head #:: makeLazyList(s.tail.filter(s.head + 1 < _))

makeLazyList(LazyList.from(0)).take(3).toList

val l: LazyList[Int] => LazyList[Int] = (s: LazyList[Int]) =>
  s.head #:: l(s.tail.filter(s.head + 1 < _))

l(LazyList.from(0)).take(3).toList


def makeList(s: List[Int]): List[Int] =
  s.head :: makeList(s.tail)

makeList(List(1,2,3,4,5))

