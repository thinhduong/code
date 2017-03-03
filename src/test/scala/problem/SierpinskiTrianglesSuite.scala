package problem

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SierpinskiTrianglesSuite extends FunSuite{
  test("with n = 0") {
    SierpinskiTriangles.drawTriangles(5)
  }
}
