package dynamic

object NikitaAndTheGame {
  def solve(n: Int, xs: Seq[Int]): Int = {
    val s = Array.fill(n, n)(0L)
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
            val half = s(j)(j + i) / 2
            val idx = (0 until i).indexWhere(k => s(j)(k) == half)

            if (idx == -1)
              p(j)(j + i) = 0
            else
              p(j)(j + i) = math.max(p(j)(idx),  p(idx+1)(j+i)) + 1
          }
        }
      }                                                                       }

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
