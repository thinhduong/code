import scala.io.Source
import java.io._

object Test {
  def do1() = {
    val filename = "D:\\bk\\example.txt"
    val ret = Source.fromFile(filename).
      getLines().
      map(x => {
        val split = x.split(',')
        println(s"${split(0).substring(1, 8)}${split(0).substring(12, 18)}")
        (s"${split(0).substring(1, 9)}${split(0).substring(12, 18)}", split(1).substring(0, split(1).length - 1).toLong)
      }).
      toList.
      groupBy(_._1).
      mapValues(_.map(x => x._2).sum).
      toArray

    printToFile(new File("D:\\bk\\result_count.txt")) { p =>
      ret.foreach(p.println)
    }
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
