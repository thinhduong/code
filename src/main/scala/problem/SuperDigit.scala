package problem

object Solution {
  def main(args: Array[String]): Unit = {
    val inputs = readLine().split(' ')
    val (n, k) = (inputs(0).toList.map(_.toInt).sum, inputs(1).toInt)

    def go(n: Long, acc: Long): Long = {
      if (n < 10)
        acc + n
      else
        go(n / 10, acc + (n % 10))
    }

    def go1(n: Long): Long = {
      if (n < 10)
        n
      else go1(go(n, 0))
    }

    println(go1(n * k))
  }
}
