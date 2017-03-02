package problem

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SuperDigitSuite extends FunSuite{
  test("with maximum N and k") {
    val n = "9" * 100000
    val k = "100000"

    assert(SuperDigit.solve(n, k) == "9")
  }

  test("test 1") {
    val n = "148"
    val k = "3"

    assert(SuperDigit.solve(n, k) == "3")
  }
}
