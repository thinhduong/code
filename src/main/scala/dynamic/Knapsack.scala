package dynamic

object Knapsack {
  def solve(xs: Seq[Int], k: Int): Int = {
    val arr = Array.fill(k + 1)(0)
    var ret = 0

    for(i <- (1 to k)) {
      val xsLessThanI = xs.filter(_ <= i).filter(x => (arr(i - x) > 0 && arr(x) > 0) || i == x)

      if (!xsLessThanI.isEmpty) {
        arr(i) = xsLessThanI.map(x => arr(i - x) + arr(x)).min + 1

        if (arr(i) != 0)
          ret = i
      }
    }

    ret
  }

  def main(args: Array[String]): Unit = {
    val t = readInt()

    for (i <- (0 until t)) {
      val k = readLine().split(' ')(1).toInt
      val xs = readLine().split(' ').map(_.toInt)

      println(solve(xs, k))
    }
  }
}
