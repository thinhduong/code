package intern

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class PhoneNumberSuite extends FunSuite {
  test("test 0") {
    val a = (0 until 25000000).foldLeft(mutable.Map[String, String]())((map, x) => {
      map += x.toString -> x.toString
    })

    val b = 0
  }

  test("test 1") {
    val ret = PhoneNumber.solve("0126 449 3937")
    assert(ret.length == 1 && ret.head == "01264493937")
  }

  test("test 2") {
    val ret = PhoneNumber.solve("SDT:0126 449 3937")
    assert(ret.length == 1 && ret.head == "01264493937")
  }

  test("test 3") {
    val ret = PhoneNumber.solve("Toi dang xai 2 so 0126 449 3937 va 09 85 9192 72")
    assert(ret.length == 2 && ret.last == "01264493937" && ret.head == "0985919272")
  }

}
