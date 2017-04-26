package dynamic

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class KnapsackSuite extends FunSuite {
  test("test 1") {
    val ret = Knapsack.solve(List(5), 6)
    assert(ret === 5)
  }

  test("test 2") {
    val ret = Knapsack.solve(List(3,3,3,3,3,3), 8)
    assert(ret === 6)
  }

  test("test 3") {
    val ret = Knapsack.solve(List(9, 4, 4, 9, 4, 9, 9, 9, 9), 10)
    assert(ret === 9)
  }

  test("test 4") {
    val ret = Knapsack.solve(List(3, 4, 4, 4, 8), 9)
    assert(ret === 9)
  }
}
