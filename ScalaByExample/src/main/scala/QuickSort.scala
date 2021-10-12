import Test.estimatePerformance

object QuickSort {

  def main(args: Array[String]): Unit = {
    val r = scala.util.Random
    val arr = (for (_ <- 1 to 10000) yield r.nextInt(10000)).toArray

    estimatePerformance(() => java.util.Arrays.sort(arr))
    estimatePerformance(() => sort(arr.toList))
    estimatePerformance(() => sortSimple(arr))
  }


  /**
   * base case: if xs.length <= 1 then itself
   * 1. pick the middle of array as pivot
   * 2. partition xs into lessThanPivot and equalToPivot and greaterThanPivot
   * 3. call each recursive function in left and right
   * 4. concat left and middle and right
   */
  def sort(xs: List[Int]): List[Int] = {
    if (xs.length <= 1) xs
    else {
      val (lessThanPivot, equalToPivot, greaterThanPivot) = partition(xs)
      sort(lessThanPivot) ::: equalToPivot ::: sort(greaterThanPivot)
    }
  }

  def partition(xs: List[Int]): (List[Int], List[Int], List[Int]) = {
    val pivot = xs(xs.length / 2)

    xs.foldLeft((List[Int](), List[Int](), List[Int]())) {
      case ((left, mid, right), ele) =>
        ele match {
          case e if e < pivot => (e :: left, mid, right)
          case e if e > pivot => (left, mid, e :: right)
          case e => (left, e :: mid, right)
        }
    }
  }


  def sortSimple(xs: Array[Int]): Array[Int] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)
      Array.concat(
        // by going through Array 3 times
        sortSimple(xs filter (pivot > _)),
        xs filter (pivot == _),
        sortSimple(xs filter (pivot < _)))
    }
  }


  def sortImperative(xs: Array[Int]): Unit = {
    def swap(i: Int, j: Int): Unit = {
      val t = xs(i)
      xs(i) = xs(j)
      xs(j) = t
    }

    def recFuc(l: Int, r: Int): Unit = {
      val pivot = xs((l + r) / 2)
      var i = l
      var j = r
      while (i <= j) {
        while (xs(i) < pivot) i += 1
        while (xs(j) > pivot) j -= 1
        if (i <= j) {
          swap(i, j)
          i += 1
          j -= 1
        }
      }
      if (l < j) recFuc(l, j)
      if (j < r) recFuc(i, r)
    }

    recFuc(0, xs.length - 1)
  }


}
