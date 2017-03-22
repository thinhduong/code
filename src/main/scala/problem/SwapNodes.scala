package problem

object SwapNodes {

  trait BTNode

  class Node(var left: BTNode, val value: Int, var right: BTNode, val depth: Int) extends BTNode

  object Node {
    def unapply(arg: Node): Option[(BTNode, Int, BTNode, Int)] = Some(arg.left, arg.value, arg.right, arg.depth)
    def apply(left: BTNode, value: Int, right: BTNode, depth: Int) = new Node(left, value, right, depth)
  }

  object Empty extends BTNode

  def buildTree(rawNode: (Int, Int, Int), rawNodes: Seq[(Int, Int, Int)]): BTNode = {
    def go(node: (Int, Int, Int), depth: Int): BTNode = node match {
      case (v, -1, -1) => Node(Empty, v, Empty, depth + 1)
      case (v, l, -1) => Node(go(rawNodes(l - 1), depth + 1), v, Empty, depth + 1)
      case (v, -1, r) => Node(Empty, v, go(rawNodes(r - 1), depth + 1), depth + 1)
      case (v, l, r) => Node(go(rawNodes(l - 1), depth + 1), v, go(rawNodes(r - 1), depth + 1), depth + 1)
    }

    go(rawNode, 0)
  }

  def traverse(root: BTNode): List[BTNode] = root match {
    case Node(Empty, _, Empty, _) => List(root)
    case Node(l, _, Empty, _) => traverse(l) ++ List(root)
    case Node(Empty, _, r, _) => List(root) ++ traverse(r)
    case Node(l, _, r, _) => traverse(l) ++ List(root) ++ traverse(r)
  }

  def swap(k: Int, maxDepth: Int, root: BTNode, nodes: List[BTNode]): BTNode = {
    val depths = (k to maxDepth by k)

    nodes.filter{
      case Node(_, _, _, d) => depths.contains(d)
    }.map {
      case x: Node => {
        val tmp = x.left
        x.left = x.right
        x.right = tmp
      }
    }

    root
  }

  def main(args: Array[String]): Unit = {
    val n = readInt()

    val inputs = (0 until n).map(i => {
      val ins = readLine().split(' ')
      (i, ins(0).toInt, ins(1).toInt)
    })

    val root = buildTree(inputs(0), inputs)
    val nodes = traverse(root)
    val maxDepth = nodes.map { case Node(_, _, _, depth) => depth }.max

    val t = readInt()

    val ks = (0 until t).map(_ => readInt())

    ks.foreach(k => {
      swap(k, maxDepth, root, nodes)
      println(traverse(root).map { case SwapNodes.Node(_,v,_,_) => v }.mkString(" "))
    })
  }
}
