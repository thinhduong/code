package euleur

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Euler174Suite  extends FunSuite {
  test("test 1") {
    val actual = Euler174.solve(100)
    assert(actual === 24)
  }
}
