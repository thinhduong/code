package dynamic

object StockMaximize {
  def solve(n: Int, xs: Seq[Int]): Long = {
    if (xs.isEmpty)
      0
    else {
      val maxIdx = xs.indexOf(xs.max)
      val buyBeforeMaxPrice: Long = xs.take(maxIdx).sum
      val profit: Long = (xs(maxIdx).toLong * maxIdx - buyBeforeMaxPrice)

      profit + solve(n, xs.drop(maxIdx + 1))
    }
  }

  def main(args: Array[String]): Unit = {
    val t = readInt()

    for(ele <- (0 until t)) {
      val n = readInt()
      val xs = readLine().split(' ').map(_.toInt)

      println(solve(n, xs))
    }
  }
}
