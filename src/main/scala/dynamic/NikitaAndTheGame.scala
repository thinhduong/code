package dynamic

object NikitaAndTheGame {
  def solve(n: Int, xs: Seq[Int]): Int = {
    val s = Array.fill(n, n)(0)
    val p = Array.fill(n, n)(0)

    for (i <- (0 until n))
      s(i)(i) = xs(i)

    for (i <- (1 until n)){
      for (j <- (0 until n)) {
        if (j + i < n) {
          s(j)(j + i) = s(j)(j + i - 1) + s(j + i)(j + i)

          if (s(j)(j + i) % 2 == 1)
            p(j)(j + i) = 0
          else {
            val xs = (0 until i - 1).filter(k => s(j)(k) == s(k+1)(j+i))

            if (xs.isEmpty)
              p(j)(j + i) = 0
            else
              p(j)(j + i) = xs.map(k => math.min(p(j)(k), p(k+1)(j+i)) + 1).max
          }
        }
      }
    }

    p(0)(n-1)
  }

  def main(args: Array[String]): Unit = {
    val t = readInt()

    for (i <- (0 until t)) {
      val n = readInt()
      val xs = readLine().split(' ').map(_.toInt)
      println(solve(n, xs))
    }
  }
}
