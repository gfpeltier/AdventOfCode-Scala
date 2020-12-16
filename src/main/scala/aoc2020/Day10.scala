package aoc2020

import scala.collection.mutable.ListBuffer

object Day10 {

  def buildChain(adapters: Set[Int]): (List[Int], Int, Int, Int) = {
    var chain = ListBuffer.empty[Int]
    val diffCnts = Array.fill[Int](4)(0)
    var currJ, nextJ = 0
    var remAdapters = adapters
    while (remAdapters.nonEmpty) {
      nextJ = ((currJ - 3) to (currJ + 3)).toSet.intersect(remAdapters).toList.min
      val diff = Math.abs(nextJ - currJ)
      diffCnts(diff) = diffCnts(diff) + 1
      currJ = nextJ
      remAdapters -= nextJ
      chain += nextJ
    }
    (chain.toList, diffCnts(1), diffCnts(2), diffCnts(3))
  }

  def part1(instrs: List[String]): Int = {
    val nums = instrs.map(_.toInt).toSet
    val (chain, oneJ, twoJ, threeJ) = buildChain(nums)
    println(chain)
    oneJ * (threeJ + 1)
  }

  def part2(instrs: List[String]): Long = {
    instrs.map(_.toInt).sorted
      .foldLeft(Map[Int, Long](0 -> 1))((m, n) => {
        m + (n -> (m.getOrElse(n-1, 0L) + m.getOrElse(n-2, 0L) + m.getOrElse(n-3, 0L)))
      })
      .max._2
  }
}
