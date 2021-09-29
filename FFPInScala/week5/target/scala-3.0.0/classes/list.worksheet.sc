val xs1 = List(1, 3, 5, 7)

xs1.last
xs1.init

val xs2 = List(2, 4 ,6 ,8)
xs1 ++ xs2
xs1 ::: xs2

xs2.updated(0, 0)

if true then true else false

xs2.reverse


extension [T](xs: List[T])
  // O(xs.length)
  // can be tailrec
  def ++ (ys: List[T]): List[T] =
    xs match
      case Nil => ys
      case x :: xs1 => x :: (xs1 ++ ys)

xs1 ++ xs2