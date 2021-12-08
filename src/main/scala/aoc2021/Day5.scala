package aoc2021

import scala.util.chaining.scalaUtilChainingOps

object Day5 {

  case class Point(x: Int, y: Int)

  case class Line(start: Point, end: Point)

  def isVertical(line: Line): Boolean = line.start.x == line.end.x

  def isHorizontal(line: Line): Boolean = line.start.y == line.end.y

  def parsePoint(ps: String): Point = ps.split(',').map(_.toInt).pipe(nm => Point(nm.head, nm.last))

  def parseLine(ls: String): Line = ls.split(" -> ").map(parsePoint).pipe(pts => Line(pts.head, pts.last))

  def part1(inLines: Seq[String]): Int = {
    val lines = inLines.map(parseLine)
    lines
      .filter(l => isVertical(l) || isHorizontal(l))
      .foldLeft(Map.empty[(Int, Int), Int])((m, l) => {
        val pts = if (isVertical(l)) {
          val rng = if (l.end.y > l.start.y) l.start.y to l.end.y else l.start.y to l.end.y by -1
          rng.map((l.start.x, _))
        } else {
          val rng = if (l.end.x > l.start.x) l.start.x to l.end.x else l.start.x to l.end.x by -1
          rng.map((_, l.start.y))
        }
        pts.foldLeft(m)((mm, pt) => mm.updatedWith(pt)(oi => Some(oi.getOrElse(0) + 1)))
      })
      .values
      .count(_ > 1)
  }

  def part2(inLines: Seq[String]): Int = {
    val lines = inLines.map(parseLine)
    lines
      .foldLeft(Map.empty[(Int, Int), Int])((m, l) => {
        val pts = if (isVertical(l)) {
          val rng = if (l.end.y > l.start.y) l.start.y to l.end.y else l.start.y to l.end.y by -1
          rng.map((l.start.x, _))
        } else if (isHorizontal(l)) {
          val rng = if (l.end.x > l.start.x) l.start.x to l.end.x else l.start.x to l.end.x by -1
          rng.map((_, l.start.y))
        } else {
          val xrng = if (l.end.x > l.start.x) l.start.x to l.end.x else l.start.x to l.end.x by -1
          val yrng = if (l.end.y > l.start.y) l.start.y to l.end.y else l.start.y to l.end.y by -1
          xrng.zip(yrng)
        }
        pts.foldLeft(m)((mm, pt) => mm.updatedWith(pt)(oi => Some(oi.getOrElse(0) + 1)))
      })
      .values
      .count(_ > 1)
  }

}
