package problem

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class PentagonalNumbersSuite extends FunSuite {
  test("with n = 0") {
    val ns = (1 to 5)
    val ret = PentagonalNumbers.solve(ns.max)

    for (elem <- ns.map(x => ret(x - 1)).zip(List(1, 5, 12, 22, 35))) {
      assert(elem._1 === elem._2)
    }
  }

  test("performance") {
    val a = math.pow(10, 5).toInt
    val rnd = new Random()
    val ns = (0 to a).map(_ => rnd.nextInt(a + 1))

    TimeHelper.time {
      val ret = PentagonalNumbers.solve(ns.max)
      ns.map(x => ret(x - 1))
    }
  }
}
