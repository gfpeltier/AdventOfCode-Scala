package aoc2019

import scala.collection.mutable

class TreeNode(val value: String) {
  var parent: String = _
  var children: Set[TreeNode] = Set.empty
}

object Day6 {

  def appendOrbit(tree: mutable.Map[String, TreeNode], parent: String, child: String): mutable.Map[String,TreeNode] = {
    val tnode = if (tree.contains(child)) tree(child) else new TreeNode(child)
    tnode.parent = parent
    if (!tree.contains(parent)) {
      val pnode = new TreeNode(parent)
      tree += (parent -> pnode)
    }
    tree(parent).children += tnode
    tree += (child -> tnode)
  }

  def buildTree(orbits: List[String]): mutable.Map[String, TreeNode] = {
    var tree = mutable.Map[String, TreeNode]()
    val orbParse = raw"([A-Z0-9]+)\)([A-Z0-9]+)".r
    for (o <- orbits) {
      tree = o match {
        case orbParse(p1, p2) => appendOrbit(tree, p1, p2)
      }
    }
    tree
  }

  def getNodeDepth(tree: mutable.Map[String, TreeNode], node: String): Int = {
    var depth = 0
    var cnode = node
    while (cnode != "COM") {
      cnode = tree(cnode).parent
      depth += 1
    }
    depth
  }

  def part1(orbits: List[String]): Int = {
    val tree = buildTree(orbits)
    println(tree.size)
    val leafNodes = tree.filter(n => n._2.children.isEmpty).foldLeft(List[String]())((l, n) => l :+ n._1)
    leafNodes.map(nk => getNodeDepth(tree, nk)).map(d => d*(d+1)/2).sum
  }
}
