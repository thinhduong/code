package problem

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import problem.SwapNodes._

@RunWith(classOf[JUnitRunner])
class SwapNodesSuite extends FunSuite{
  test("build tree") {
    val inputs = List((1, 2, 3), (2, -1, 4), (3, -1, 5), (4, -1, -1), (5, -1, -1))
    val root = SwapNodes.buildTree(inputs(0), inputs)
    val nodes = SwapNodes.traverse(root).map { case SwapNodes.Node(_,v,_,_) => v }

    assert(nodes === List(2, 4, 1, 3, 5))
  }

  test("swap 1") {
    val inputs = List((1, 2, 3), (2, -1, 4), (3, -1, 5), (4, -1, -1), (5, -1, -1))

    val root = buildTree(inputs(0), inputs)
    val nodes = traverse(root)
    val maxDepth = nodes.map { case Node(_, _, _, depth) => depth }.max

    swap(2, maxDepth, root, nodes)
    val afterSwap1 = traverse(root).map { case SwapNodes.Node(_,v,_,_) => v }

    assert(afterSwap1 == List(4,2,1,5,3))
  }

  test("swap 2") {
    val inputs = List((1, 2, 3), (2, 4, -1), (3, 5, -1), (4, 6, -1), (5, 7, 8), (6, -1, 9), (7, -1, -1), (8, 10, 11), (9, -1, -1), (10, -1, -1), (11, -1, -1))

    val root = buildTree(inputs(0), inputs)
    val nodes = traverse(root)
    val maxDepth = nodes.map { case Node(_, _, _, depth) => depth }.max

    swap(2, maxDepth, root, nodes)
    val afterSwap1 = traverse(root).map { case SwapNodes.Node(_,v,_,_) => v }

    assert(afterSwap1.mkString(" ") == "2 9 6 4 1 3 7 5 11 8 10")

    swap(4, maxDepth, root, nodes)
    val afterSwap2 = traverse(root).map { case SwapNodes.Node(_,v,_,_) => v }

    assert(afterSwap2.mkString(" ") == "2 6 9 4 1 3 7 5 10 8 11")
  }
}
