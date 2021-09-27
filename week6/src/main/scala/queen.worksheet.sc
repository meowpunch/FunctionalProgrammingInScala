import scala.annotation.tailrec

/*
 * xqxx
 * xxxq
 * qxxx
 * -> kth
 *
 */
def isSafe(col: Int, queens: List[Int]): Boolean = {
  def loop(restQueens: List[Int], deltaRow: Int): Boolean =
    restQueens match {
      case Nil => true
      case c :: rq =>
        // vertical and diagonal
        if col == c || (col - c).abs == deltaRow then false
        else loop(rq, deltaRow + 1)
    }

  loop(queens, 1)
}


def queen(n: Int): Set[List[Int]] =
  @tailrec
  // k => row
  def loop(k: Int, acc: Set[List[Int]]): Set[List[Int]] =
//    println(acc)
    if (k == 0) acc
    else
      loop(
        k - 1,
        for {
          queens: List[Int] <- acc
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
      )

  loop(n, Set(List()))


queen(4)