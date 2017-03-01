package problem

object Solution {
  def main(args: Array[String]): Unit = {
    val inputs = readLine().split(' ').map(_.toInt)
    val (n, k) = (inputs(0), inputs(1))

    def sumOfN(n: Int, acc: Int): Int = {
      if (n < 10)
        acc
      else
        sumOfN(n / 10, acc + (n % 10))
    }

    def go(n: Int, k: Int): Int = {
      if (n * k < 10) n * k
      else go(sumOfN(n, 0), 1)
    }

    println(go(n, k))
  }
}
