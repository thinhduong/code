package problem

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunctionsAndFractalsSuite extends FunSuite {
  test("with n = 0") {
    FunctionsAndFractals.draw(5)
  }
}
