package dynamic

object NikitaAndTheGame {
  def build(n: Int, xs: Seq[Int]): Array[Array[Int]] = {
    val arr = Array.fill(n, n)(0)

    for (i <- (0 until n))
      arr(i)(i) = xs(i)

    for (i <- (0 until n)){
      for (j <- (i + 1 until n)) {
         arr(i)(j) = arr(i)(j - 1) + arr(j)(j)
      }
    }

    arr
  }

  def solve(n: Int, xs: Seq[Int]): Int = {
    def go(from: Int, to: Int, sum: Int, mem: Array[Array[Int]]): Int = {
      if (to - from < 1 || sum % 2 == 1)
        0
      else {
        val half = sum / 2

        val idx = mem(from).indexOf(half)

        if (idx == -1)
          0
        else {
          val l = go(from, idx, half, mem)
          val r = go(idx + 1, to, half, mem)

          math.max(l, r) + 1
        }
      }
    }

    val sumOfXs = xs.sum
    if (n < 2 || sumOfXs % 2 == 1)
      0
    else {
      val mem = build(n, xs)

      go(0, n - 1, sumOfXs, mem)
    }
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
