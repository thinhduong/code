package problem

object FunctionsAndFractals {
  val noOfRows = 63
  val noOfCols = 100

  private case class Fractal(startX: Int, startY: Int, height: Int)

  def draw(n: Int) = {
    def go(i: Int, acc: List[Fractal]): List[Fractal] = {
      if (i == n)
        acc
      else {
        val needGen = acc.take(Math.pow(2, i).toInt)
        val newAcc = needGen.flatMap(x => List(Fractal(x.startX - x.height * 2, x.startY - x.height, x.height / 2),
                                               Fractal(x.startX - x.height * 2, x.startY + x.height, x.height / 2))) :::  acc
        go(i+1, newAcc)
      }
    }

    val fractals = go(1, List(Fractal(noOfRows - 1, noOfCols/2 - 1, 16)))
    val arr = Array.fill[Char](noOfRows, noOfCols)('_')

    fractals.map(x => {
      (0 to x.height - 1).map(i => {
        arr(x.startX - i)(x.startY) = '1'
        arr(x.startX - x.height - i)(x.startY - i - 1) = '1'
        arr(x.startX - x.height - i)(x.startY + i + 1) = '1'
      })
    })

    (0 to noOfRows - 1).map(i => println(arr(i).mkString))
  }

  def main(args: Array[String]): Unit = {
    draw(readInt())
  }
}
