import scala.annotation.tailrec
// higher-order functions and currying

def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) then 0 else f(a) + sum(f)(a + 1, b)

sum(identity)(1, 10)

@tailrec
def tailSum(f: Int => Int)(a: Int, b: Int, acc: Int): Int =
  if (a > b) then acc else tailSum(f)(a + 1, b, acc + f(a))

tailSum(identity)(1, 10, 0)

def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) then 1 else f(a) * product(f)(a + 1, b)

def mapReduce(map: Int => Int, reduce: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if (a > b) then zero
  else reduce(map(a), mapReduce(map, reduce, zero)(a + 1, b))

def mapReduceWithInnerFunction(map: Int => Int, reduce: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  def innerFunction(a: Int, b: Int): Int =
    if (a > b) then zero
    else reduce(map(a), innerFunction(a + 1, b))

  innerFunction(a, b)

def mapReduceTailRec(map: Int => Int, reduce: (Int, Int) => Int)(a: Int, b: Int, acc: Int): Int =
  @tailrec
  def recFun(a: Int, b: Int, acc: Int): Int =
    if (a > b) then acc
    else recFun(a + 1, b, reduce(acc, map(a)))

  recFun(a, b, acc)


def sumF(f: Int => Int) /*(a: Int, b: Int)*/ = mapReduce(f, (x, y) => x + y, 0) /*(a, b)*/
sumF(identity)(1, 10)


def productF(f: Int => Int) = mapReduceTailRec(f, (x, y) => x * y)

def fact(n: Int) = productF(identity)(1, n, 1)

fact(5)

def estimatePerformace(f: () => Any) =
  val t1 = System.nanoTime
  f()
  (System.nanoTime - t1) / 1e9d


println(
  estimatePerformace(() => mapReduceTailRec(identity, (x, y) => x + y)(1, 100000, 0))
)
println(
    estimatePerformace(() => mapReduce(identity, (x, y) => x + y, 0)(1, 100000))
)
println(
    estimatePerformace(() => mapReduceWithInnerFunction(identity, (x, y) => x + y, 0)(1, 100000))
)
