package dynamic

object RedJohnIsBack {
  def solve(n: Int): Long = {
    val arr = Array.fill(n+1)(0L)

    for(i <- (1 to 3)) {
      arr(i) = 1
    }

    arr(4) = 2

    for(i <- (5 to n)) {
      arr(i) = arr(i - 1) + arr(i - 4)
    }

    arr(n)
  }

  def main(args: Array[String]): Unit = {
    val t = readInt()

    for (i <- (0 until t)) {
      println(solve(readInt()))
    }
  }
}
