package problem

object SuperDigit {
  def solve(n0: String, k0: String): String = {
    def go(n: String): String = {
      if (n.length == 1)
        n
      else go(n.toList.map(_ - '0').sum.toString)
    }

    val (n, k) = (n0.toList.map(_ - '0').sum, k0.toInt)
    go(n.toString *k)
  }

  def main(args: Array[String]): Unit = {
    val inputs = readLine().split(' ')

    println(solve(inputs(0), inputs(1)))
  }
}
