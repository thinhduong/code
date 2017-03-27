package problem

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import problem.PrisonTransport._

import scala.io.Source

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

  test("case 3") {
    val ret = solve(8, List((0, 1), (0, 3), (0, 2), (0, 4)))

    assert(ret == 6)
  }

  test("case 4") {
    val filename = "prisonTransfer.txt"
    val resource = this.getClass.getClassLoader.getResource(filename)

    val couples = Source.fromFile(resource.toURI).getLines().drop(2).map(l => {
      val tmp = l.split(' ').map(_.toInt - 1)
      (tmp(0), tmp(1))
    }).toSeq

    val ret = solve(100000, couples)
    assert(ret == 19160)
  }

  test("case 5") {
    val filename = "prisonTransfer1.txt"
    val resource = this.getClass.getClassLoader.getResource(filename)

    val couples = Source.fromFile(resource.toURI).getLines().drop(2).map(l => {
      val tmp = l.split(' ').map(_.toInt - 1)
      (tmp(0), tmp(1))
    }).toSeq

    val ret = solve(8, couples)
    assert(ret == 6)
  }
}
