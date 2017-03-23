package problem

object PrisonTransport {
  def label(p: Int, xs: Array[Int]): Int =
    if (p == xs(p)) p else label(xs(p), xs)

  def connect(p: Int, q: Int, xs: Array[Int]): Unit = {
    val pLabel = label(p, xs)
    val qLabel = label(q, xs)

    if (label(p, xs) != label(q, xs))
      xs(qLabel) = pLabel
  }

  def calculate(xs: Array[Int]): Int = {
    val a = xs.groupBy(x => x).
      mapValues(_.length).
      map(x => {
        val sqrtOfX = math.sqrt(x._2)
        math.ceil(sqrtOfX).toInt
      })

    a.foldLeft(0)(_ + _)
  }

  def solve(n: Int, couples: Seq[(Int, Int)]): Int = {
    val ls = new Array[Int](n)
    (0 until n).foreach(i => ls(i) = i)

    couples.foreach{
      case(p, q) => connect(p, q, ls)
    }

    calculate(ls)
  }

  def main(args: Array[String]): Unit = {
    val n = readInt()
    val m = readInt()

    val couples = (0 until m).map(_ => {
      val in = readLine().split(' ').map(_.toInt - 1)
      (in(0), in(1))
    })

    print(solve(n, couples))
  }
}
