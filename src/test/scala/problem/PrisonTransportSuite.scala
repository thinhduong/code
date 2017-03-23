package problem

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import problem.PrisonTransport._

@RunWith(classOf[JUnitRunner])
class PrisonTransportSuite extends FunSuite{
  test("case 1") {
    val ret = solve(4, List((0, 1), (0, 3)))

    assert(ret == 3)
  }

  test("case 2") {
    val ret = solve(100000, List())

    assert(ret == 100000)
  }


}
