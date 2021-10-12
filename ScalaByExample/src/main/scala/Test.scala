object Test {

  def main(args: Array[String]): Unit = {
    val t1 = (10000000 to 1).toList

    estimatePerformance(() => t1.sorted)
    estimatePerformance(() => t1.sorted)
    estimatePerformance(() => t1.sorted)
  }

  def estimatePerformance(f: () => Any): Unit = {
    val start = System.nanoTime()
    f()
    println(s"time: ${System.nanoTime() - start}")

  }
}
