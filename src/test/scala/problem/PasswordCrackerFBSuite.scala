package problem

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PasswordCrackerFBSuite extends FunSuite {
  test("test 1") {
    val ret = PasswordCrackerFP.solve(List("because", "can", "do", "must", "we", "what"), "wedowhatwemustbecausewecan")
    assert(ret === "we do what we must because we can")
  }

  test("test 2") {
    val ret = PasswordCrackerFP.solve(List("hello", "planet"), "helloworld")
    assert(ret === "WRONG PASSWORD")
  }

  test("test 3") {
    val ret = PasswordCrackerFP.solve(List("ab", "abcd", "cd"), "abcd")
    assert(ret === "ab cd")
  }
}
