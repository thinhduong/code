package dynamic

object RedJohnIsBack {
  def solve(n: Int): Long = {
    def go(m: Int): Int = {
      if (m == 1)
        0
      else if (m == 2)
        1
      else if (m == 3)
        2
      else {
        var c = 2
        for (i <- (5 to m)) {
          var isPrime = true
          for (j <- (2 to math.sqrt(m).toInt + 1)) {
            if (i % j == 0)
              isPrime = false
          }

          if (isPrime)
            c += 1
        }
        c
      }
    }

    if (n <= 3)
      0
    else {
      val arr = Array.fill(n + 1)(0)

      for (i <- (1 to 3)) {
        arr(i) = 1
      }

      arr(4) = 2

      for (i <- (5 to n)) {
        arr(i) = arr(i - 1) + arr(i - 4)
      }

      go(arr(n))
    }
  }

  def main(args: Array[String]): Unit = {
    val t = readInt()

    for (i <- (0 until t)) {
      println(solve(readInt()))
    }
  }
}
