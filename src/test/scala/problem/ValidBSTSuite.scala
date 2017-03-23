package problem

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import problem.ValidBST._

@RunWith(classOf[JUnitRunner])
class ValidBSTSuite extends FunSuite{
  test("test 1") {
    val eleStr = "1 2 3"

    val root = eleStr.
      split(' ').
      map(x => x.toInt).
      foldLeft(Empty: BST)((tree, ele) => tree.insert(ele))

    val ret = traverse(root).mkString(" ")

    assert(ret === eleStr)
  }

  test("test 2") {
    val eleStr = "3 2 1 5 4 6"

    val root = eleStr.
      split(' ').
      map(x => x.toInt).
      foldLeft(Empty: BST)((tree, ele) => tree.insert(ele))

    val ret = traverse(root).mkString(" ")

    assert(ret === eleStr)
  }

  test("test 3") {
    val eleStr = "3 4 5 1 2"

    val root = eleStr.
      split(' ').
      map(x => x.toInt).
      foldLeft(Empty: BST)((tree, ele) => tree.insert(ele))

    val ret = traverse(root).mkString(" ")

    assert(ret != eleStr)
  }
}
