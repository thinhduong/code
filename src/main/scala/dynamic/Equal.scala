package dynamic



object Equal {
  def solve(n: Int, xs: Array[Int]): Int = {
    0
  }

  def main(args: Array[String]): Unit = {
    val t = readInt()

    for (elem <- (0 until t)) {
      val n = readInt()
      val xs = readLine().split(' ').map(_.toInt)

      println(solve(n, xs))
    }
  }
}
