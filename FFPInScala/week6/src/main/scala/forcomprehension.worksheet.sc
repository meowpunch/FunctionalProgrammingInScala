def isPrime(n: Int): Boolean =
  !(2 until n).exists(x => n % x == 0)

def solutionWithFlatMap(n: Int): Any =
  (1 until n)
    .flatMap(i => (1 until i).map(j => (j, i)))
    .filter { case (x, y) => isPrime(x + y) }


def solutionWithForComp(n: Int): Any =
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (j, i)


solutionWithForComp(5)
solutionWithFlatMap(5)