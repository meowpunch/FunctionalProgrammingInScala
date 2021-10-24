

val primes: LazyList[Int] = 2 #:: LazyList.from(3).filter(
  x => {
    println(s"$x is computed")
    val cand = primes.takeWhile(_ <= math.sqrt(x))
    println(s"${cand.toList} is primes less than sqrt $x")
    cand.forall(x % _ != 0)
  }
)

primes.take(3).size

primes.takeWhile(_ < 10).foldLeft(0)((a, _) => a + 1)


lazy val primess: LazyList[Int] = LazyList.from(2).filter(
  // a derivation thereof has no more elements
  x => primess.takeWhile(_ <= math.sqrt(x)).forall(x % _ != 0)
)

primess.take(10).size // => doesn't work


def sieve(xs: LazyList[Int]): LazyList[Int] =
  xs.head #:: sieve(xs.tail.filter(_ % xs.head != 0))

sieve(LazyList.from(2)).takeWhile(_ < 30).size

LazyList.empty == LazyList()