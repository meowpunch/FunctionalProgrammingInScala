Seq("Meow", "Punch").flatMap(_.toLowerCase())

Seq(1, 2, 3).flatMap(x => List(x - 1, x, x + 1))

Seq(Seq(1, 2), 3).flatMap{
  case x: Int => List(x - 1, x, x + 1)
  case Seq(a, b) => List(a, b)
}

Seq(1,2,3).map(_ + 2)

(1 to 10 by 2) flatMap (x => (0 to 10 by 2) map (y => (x, y)))

def isPrime(n: Int): Boolean =
  (2 until n).forall(x => n % x != 0)

def isPrimeEfficient(n: Int): Boolean =
  !(2 until n).exists(x => n % x == 0)

isPrime(15)
isPrime(17)