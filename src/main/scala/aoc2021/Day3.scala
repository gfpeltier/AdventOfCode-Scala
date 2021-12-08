package aoc2021

import scala.util.chaining._

object Day3 {

  def part1(lines: Seq[String]): Int = {
    lines.foldLeft(List.fill(lines.head.length)((0, 0)))((ccnts, line) =>
      line.zipWithIndex.foldLeft(ccnts)((cnts, ci) => ci._1 match {
        case '0' => cnts.updated(ci._2, (cnts(ci._2)._1 + 1, cnts(ci._2)._2))
        case '1' => cnts.updated(ci._2, (cnts(ci._2)._1, cnts(ci._2)._2 + 1))
      })
    ).zipWithIndex.foldLeft((0, 0))((gamEps, valsI) => {
      val nGamma = if (valsI._1._2 >= valsI._1._1) gamEps._1 | (1 << lines.head.length - 1 - valsI._2) else gamEps._1
      val nEps = if (valsI._1._1 > valsI._1._2) gamEps._2 | (1 << lines.head.length - 1 - valsI._2) else gamEps._2
      (nGamma, nEps)
    }).pipe(gamEps => gamEps._1 * gamEps._2)
  }

  def part2(lines: Seq[String]): Int = ???
}
