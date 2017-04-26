package dynamic

object SherlockAndCost {
  def solve(xs: Seq[Int]): Long = {
    var sumOdd = 0L
    var sumEven = 0L

    for(i <- (1 until xs.length)) {
      if (i % 2 == 0) {
        sumEven += xs(i - 1) - 1
        sumOdd  += xs(i) - 1
      }
      else {
        sumOdd  += xs(i - 1) - 1
        sumEven += xs(i) - 1
      }
    }

    math.max(sumEven, sumOdd)
  }

  def main(args: Array[String]): Unit = {
    val t = readInt()

    for(i <- (0 until t)) {
      readInt()
      val xs = readLine().split(' ').map(_.toInt)
      println(solve(xs))
    }
  }
}
