package problem

object SierpinskiTriangles {
  def main(args: Array[String]) {
    drawTriangles(readInt())
  }

  def drawTriangles(n: Int) {
    val noOfRow = 31
    val noOfCol = 62
    var arr = Array.fill[Char](32, 63)('_')

    for{
      i <- (0 to noOfRow)
      j <- (0 to noOfCol)
    } yield()
  }
}
