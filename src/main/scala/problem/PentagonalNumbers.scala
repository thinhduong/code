package problem

object PentagonalNumbers {
  def solve(n: Int): Array[Long] = {
    val arr = new Array[Long](n + 1)
    var a = 0
    var b = 1L
    var c = 1

    (1 to n).foreach(idx => {
      if (idx == 1)
        arr(idx - 1) = 1
      else {
        a += 5
        b = a + b - c
        c += 2

        arr(idx - 1) = b
      }
    })

    arr
  }

  def main(args: Array[String]): Unit = {
    val t = readInt()

    val ns = (0 until t).map(_ => readInt())

    val ret = solve(ns.max)

    ns.foreach(x => println(ret(x - 1)))
  }
}
