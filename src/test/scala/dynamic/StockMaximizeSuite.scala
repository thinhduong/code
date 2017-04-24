package dynamic

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class StockMaximizeSuite extends FunSuite {
  test("test 1") {
    val ret = StockMaximize.solve(3, List(5, 3, 2))
    assert(ret === 0)
  }

  test("test 2") {
    val ret = StockMaximize.solve(3, List(1, 2, 100))
    assert(ret === 197)
  }

  test("test 3") {
    val ret = StockMaximize.solve(4, List(1, 3, 1, 2))
    assert(ret === 3)
  }

  test("test 4") {
    val ret = StockMaximize.solve(8, List(1, 1, 5, 2, 1000, 7, 1000, 300))
    assert(ret === 4984 )
  }

  test("test 5") {
    val filename = "stockMaximize.txt"
    val resource = this.getClass.getClassLoader.getResource(filename)

    val lines = Source.fromFile(resource.toURI).getLines().toList
    val t = lines(0).toInt

    val ret = (1 until t).map(ele => {
      val n = lines(2 * ele - 1).toInt
      val xs = lines(2 * ele).split(' ').map(_.toInt).toList
      StockMaximize.solve(n, xs)
    }).zip(List(741800583, 422883749, 483198595, 2177987236L, 1699130206, 1998861008, 1778086557, 680000410, 2239143845L, 1410726655))

     ret.foreach{ case (actual, expect) =>  assert(actual === expect)}
  }

  test("test 6") {
    val filename = "stockMaximize2.txt"
    val resource = this.getClass.getClassLoader.getResource(filename)

    val lines = Source.fromFile(resource.toURI).getLines().toList
    val t = lines(0).toInt

    val ret = (1 until t).map(ele => {
      val n = lines(2 * ele - 1).toInt
      val xs = lines(2 * ele).split(' ').map(_.toInt).toList
      StockMaximize.solve(n, xs)
    }).zip(List(747651677, 666584846, 1141122526, 2464062901L, 1203046550, 2322790380L, 730505954, 849100801, 1450140472, 872438099))

    ret.foreach{ case (actual, expect) =>  assert(actual === expect)}
  }
}
