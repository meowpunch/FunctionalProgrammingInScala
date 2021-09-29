import scala.annotation.tailrec

val xs1 = List(1, 3, 5, 7)

xs1.last
xs1.init

val xs2 = List(2, 4, 6, 8)
xs1 ++ xs2
xs1 ::: xs2

xs2.updated(0, 0)
xs2.reverse


extension[T] (xs: List[T])
// O(xs.length)
// can be tailrec
  def customConcat(ys: List[T]): List[T] =
    println(s"$xs ++ $ys")
    xs match
      case Nil => ys
      case x :: xs1 => x :: (xs1 customConcat ys)

  // O(xs.length * xs.length)
  def customReverse: List[T] =
    xs match
      case Nil => List()
      case y :: ys => ys.customReverse customConcat List(y)

  def customReverseAdvanced: List[T] =
    @tailrec
    def loop(xs: List[T], acc: List[T]): List[T] =
      xs match
        case Nil => acc
        case y :: ys => loop(ys, y :: acc)

    loop(xs, List())

xs1 customConcat xs2
xs1.customReverse
xs1.customReverseAdvanced

def removeAt[T](n: Int, xs: List[T]): List[T] =
  xs match
    case Nil => xs
    case y :: ys => if n == 0 then ys else y :: removeAt(n - 1, ys)


def removeAtTR[T](n: Int, xs: List[T]): List[T] =
  @tailrec
  def loop(n: Int, xs: List[T], acc: List[T]): List[T] =
    xs match
      case Nil => acc
      case y :: ys => if n == 0 then acc ++ ys else loop(n - 1, ys, acc ++ List(y))

  loop(n, xs, List())

removeAt(1, xs1)
removeAtTR(1, xs1)

/*
 * loop xs
 */
def flatten(xs: Any): List[Any] =
  println(xs)
  xs match
    case Nil => Nil
    case y :: ys => flatten(y) ++ flatten(ys)
    case _ => List(xs) // xs :: Nil


val ys: Any = List(List(1, 1), 2, List(3, List(5, 8)))
flatten(ys)

val xs3 = List(1, 2, 3, 4, 5, 6)
xs3.partition(_ % 2 != 0)
xs3.span(_ % 2 != 0)

def pack[T](xs: List[T]): List[List[T]] =
  xs match
    case Nil => Nil
    case y :: ys =>
      val (packed, rest) = ys.span(_ == y)
      (y :: packed) :: pack(rest)

def packTR[T](xs: List[T]): List[List[T]] =
  @tailrec
  def loop(xs: List[T], acc: List[List[T]]): List[List[T]] =
    xs match
      case Nil => acc
      case y :: ys =>
        val (packed, rest) = ys.span(_ == y)
        loop(rest, acc ++ List(y :: packed))


  loop(xs, List())


val elem = "aaabbbbcc".toList
pack(elem)
packTR(elem)

def encode[T](xs: List[T]): List[(T, Int)] =
  packTR(xs).map(x => (x.head, x.size))

encode(elem)

// advanced reverse
xs3.foldLeft(List[Int]())((x, y) => y :: x)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  xs.foldRight(List[U]())((e: T, acc: List[U]) => f(e) :: acc)

def lengthFun[T](xs: List[T]): Int =
  xs.foldRight(0)((_, acc: Int) => acc + 1)

