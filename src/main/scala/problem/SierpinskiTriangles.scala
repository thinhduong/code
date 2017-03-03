package problem

object SierpinskiTriangles {
  def main(args: Array[String]) {
    drawTriangles(readInt())
  }

  case class Triangle(startRow: Int, startCol: Int, height: Int)

  def drawTriangles(n: Int) = {
    val noOfRow = 31
    val noOfCol = 62

    val arr = Array.fill[Char](32, 63)('_')

    def go(i: Int, acc: List[Triangle]): List[Triangle] = {
      if (i == n)
        acc
      else {
        go(i + 1, acc.flatMap(x => List(Triangle(x.startRow, x.startCol, x.height / 2),
                                        Triangle(x.startRow + x.height /2 + 1, x.startCol - x.height/2 - 1, x.height/2),
                                        Triangle(x.startRow + x.height /2 + 1, x.startCol + x.height/2 + 1, x.height/2))))
      }
    }

    val triangles = go(0, List(Triangle(0, noOfCol / 2, noOfRow)))

    triangles.map(x => {
      (0 to x.height).map(i => {
        val y = i + 1
        arr(x.startRow + i)(x.startCol) = '1'
        (1 to y-1).map(z => {
          arr(x.startRow + i)(x.startCol - z) = '1'
          arr(x.startRow + i)(x.startCol + z) = '1'
        })
      })
    })

    for{
      i <- (0 to noOfRow)
    } yield ({
      println(arr(i).mkString)
    })
  }
}
