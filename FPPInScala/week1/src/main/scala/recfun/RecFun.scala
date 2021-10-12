package recfun

import scala.annotation.tailrec
import scala.collection.mutable.HashMap

object RecFun extends RecFunInterface :

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   * rCc = r-1Cc-1 + r-1Cc
   */
  def pascal(c: Int, r: Int): Int = {
    val mem = new HashMap[Tuple2[Int, Int], Int]()

    def recFun(c: Int, r: Int): Int =
      if (c == 0 || r == 0 || c == r) 1
      else mem.getOrElseUpdate((c - 1, r - 1), pascal(c - 1, r - 1)) + mem.getOrElseUpdate((c, r - 1), pascal(c, r - 1))

    recFun(c, r)
  }

  def simplePascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
   * Exercise 2
   *
   * Stack(imperative) vs Recursion(functional and delarative)
   * O(N)
   */
  def balance(chars: List[Char]): Boolean =
    def recFun(chars: List[Char], count: Int): Boolean =
      chars match {
        case Nil => count == 0
        case head :: tail => {
          head match {
            case '(' => recFun(tail, count + 1)
            case ')' => count > 0 && recFun(tail, count - 1)
            case _ => recFun(tail, count)
          }
        }
      }

    recFun(chars, 0)


  /**
   * Exercise 3
   *
   * Input:
   * coins has no duplicates? and coins are already sorted?
   */
  def countChange(money: Int, coins: List[Int]): Int =
    countChangeWithLoop(money, coins)
  //    countChangeWithoutLoop(money, coins)

  def countChangeWithLoop(money: Int, coins: List[Int]): Int =
    if (coins.isEmpty) 0
    else if (money == 0) 1
    else coins
      .filter(c => c <= money)
      .map(c => countChange(money - c, coins.filter(x => x >= c)))
      .sum

  def countChangeWithoutLoop(money: Int, coins: List[Int]): Int =
    if (money < 0) return 0
    else
      (money, coins) match {
        case (_, Nil) => 0
        case (0, _) => 1
        case (_, head :: tail) => countChange(money - head, coins) + countChange(money, coins.tail)
      }

