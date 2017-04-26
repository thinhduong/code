package dynamic

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RedJohnIsBackSuite extends FunSuite {

  test("test 1") {
    val ret = RedJohnIsBack.solve(1)
    assert(ret === 0)
  }

  test("test 2") {
    val cases = List(34, 3, 31, 35, 10, 38, 18, 27, 15, 3, 38, 14, 18, 4, 5, 23, 9, 31, 10, 25)
    val expects = List(3385, 0, 1432, 4522, 6, 10794, 42, 462, 19, 0, 10794, 15, 42, 1, 2, 155, 4, 1432, 6, 269)

    val actuals = cases.map(RedJohnIsBack.solve(_))

    actuals.zip(expects).foreach {
      case (actual, expect) => assert(actual === expect)
    }
  }
}
