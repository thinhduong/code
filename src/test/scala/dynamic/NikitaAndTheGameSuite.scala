package dynamic

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NikitaAndTheGameSuite extends FunSuite {
  /*test("test 1") {
    val ret = NikitaAndTheGame.solve(7, List(4, 1, 0, 1, 1, 0, 1))

    assert(ret === 3)
  }

  test("test 2") {
    val ret = NikitaAndTheGame.solve(3, List(3, 3, 3))

    assert(ret === 0)
  }

  test("test 3") {
    val ret = NikitaAndTheGame.solve(4, List(2, 2, 2, 2))

    assert(ret === 2)
  }      */

  test("test 4") {
    val ret = NikitaAndTheGame.solve(4, List(4, 0, 0, 0))

    assert(ret === 0)
  }
}
