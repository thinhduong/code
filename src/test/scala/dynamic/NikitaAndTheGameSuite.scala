package dynamic

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class NikitaAndTheGameSuite extends FunSuite {
  ignore("test 1") {
    val ret = NikitaAndTheGame.solve(7, List(4, 1, 0, 1, 1, 0, 1))

    assert(ret === 3)
  }

  ignore("test 2") {
    val ret = NikitaAndTheGame.solve(3, List(3, 3, 3))

    assert(ret === 0)
  }

  test("test 3") {
    val ret = NikitaAndTheGame.solve(4, List(2, 2, 2, 2))

    assert(ret === 2)
  }

  test("test 4") {
    val filename = "nikita1.txt"
    val resource = this.getClass.getClassLoader.getResource(filename)

    val lines = Source.fromFile(resource.toURI).getLines().toList
    val t = lines(0).toInt

    val ret = (1 to t).map(ele => {
      val n = lines(2 * ele - 1).toInt
      val xs = lines(2 * ele).split(' ').map(_.toLong)
      (n, xs)
    }).map { case (n, xs) => NikitaAndTheGame.solve(n, xs) }.
      zip(List(0, 1, 6, 9, 1, 3, 20, 2, 1, 2025))

    ret.map { case (actual, expect) => println(actual == expect) }
  }
}
