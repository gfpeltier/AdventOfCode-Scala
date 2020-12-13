package aoc2020

object Day13 {
  def parseOffsetBusIDs(s: String): List[(Long, Long)] = {
    s.split(',').zipWithIndex.map(bi => {
      if (bi._1 == "x") (-1.toLong, bi._2.toLong)
      else (bi._1.toLong, bi._2 % bi._1.toLong)
    }).filter(_._1 != -1).toList
  }

  def parseBusIDs(s: String): List[Int] = {
    s.split(',').filter(_.!=("x")).map(_.toInt).toList
  }

  def part1(instrs: List[String]): Int = {
    val startTime = instrs.head.toInt
    val busIDs = parseBusIDs(instrs.last)
    val nbus = busIDs.map(id => (id, id - (startTime % id))).minBy(t => t._2)
    println(nbus)
    nbus._1 * nbus._2
  }

  def part2(instrs: List[String]): Long = {
    val busIDs = parseOffsetBusIDs(instrs.last)
    var t: Long = 0
    var step: Long = 1
    for (bid <- busIDs) {
      while ((t + bid._2) % bid._1 != 0) t += step
      step *= bid._1
    }
    t
  }
}
