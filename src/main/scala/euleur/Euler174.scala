package euleur

object Euler174 {
  lazy val initArr = {
    val maxIdx = 250000
    val arr = Array.fill(maxIdx + 1)(1)

    var init = 6
    var step = 2

    while(init <= maxIdx) {

      for(i <- (init to maxIdx by step)){
        arr(i) = arr(i) + 1
      }

      step = step + 1
      init = step * (step + 1)
    }

    arr
  }

  def solve(k: Int): Int = {
    val nTake = if (k % 4 == 0) (k / 4) + 1 else k / 4
    val a = initArr.take(nTake).filter(_ <= 10)
    a.length - 2
  }

  def main(args: Array[String]) = {
    val t = readInt()

    for(i <- (0 until t)) {
      val k = readInt()

      println(solve(k))
    }
  }
}
