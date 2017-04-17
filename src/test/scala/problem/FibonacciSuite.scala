package problem

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FibonacciSuite extends FunSuite {
  test("with n = 0") {
    val ret = List(0, 1, 5, 10, 100).map(x => Fibonacci.fib(x))

    List(0, 1, 5 ,55, 24278230).zip(ret).foreach{
      case (x, y) => assert(x === y)
    }
  }
}
