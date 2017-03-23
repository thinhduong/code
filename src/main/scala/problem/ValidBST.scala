package problem

object ValidBST {
  trait BST {
    def insert(ele: Int): BST
  }

  object Empty extends BST {
    override def insert(ele: Int): BST = Node(Empty, ele, Empty)
  }

  case class Node (left: BST, value: Int, right: BST) extends BST {
    override def insert(ele: Int): BST = {
      if (ele < value)
        Node(left.insert(ele), value, right)
      else if (ele > value)
        Node(left, value, right.insert(ele))
      else
        this
    }
  }

  def traverse(root: BST): List[Int] = root match {
    case Empty => List()
    case Node(l, v, r) => List(v) ++ traverse(l) ++ traverse(r)
  }

  def main(args: Array[String]): Unit = {
    val t = readInt()
    (0 until t).foreach(_ => {
      readInt()
      val eleStr = readLine()
      val root = eleStr.
        split(' ').
        map(x => x.toInt).
        foldLeft(Empty: BST)((tree, ele) => tree.insert(ele))

      val ret = traverse(root).mkString(" ")
      println(if (ret == eleStr) "YES" else "NO")
    })
  }
}
