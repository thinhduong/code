package problem

object Fibonacci {
  def fib(n: Int): Long = {
    if (n == 0)
      0
    else if (n == 1)
      1
    else {
      var n0 = 0L
      var n1 = 1L

      (2 to n).foreach(_ => {
        val tmp = n1
        n1 = ((n0 + n1) % (math.pow(10, 8) + 7)).toLong
        n0 = tmp
      })

      n1
    }
  }

  def main(args: Array[String]): Unit = {
    val t = readInt()

    (0 until t).foreach(_ => {
      val n = readInt()
      println(fib(n))
    })
  }
}
