package aoc2021

import scala.collection.immutable.Queue
import scala.collection.mutable

object Day9 {

  def findRisk(x: Int, y: Int, lines: Seq[String]): Int = {
    val n = if (y == 0) 9 else lines(y - 1).substring(x, x + 1).toInt
    val s = if (y == lines.indices.last) 9 else lines(y + 1).substring(x, x + 1).toInt
    val e = if (x == 0) 9 else lines(y).substring(x - 1, x).toInt
    val w = if (x == lines.head.length - 1) 9 else lines(y).substring(x + 1, x + 2).toInt

    val curr = lines(y).substring(x, x + 1).toInt

    if (curr < s && curr < n && curr < e && curr < w) {
      curr + 1
    } else {
      0
    }
  }

  def part1(lines: Seq[String]): Int =
    (for {
      x <- 0 until lines.head.length
      y <- lines.indices
      n = findRisk(x, y, lines)
    } yield n).sum


  type Point = (Int, Int)

  def locateLowPts(lines: Seq[String]): Seq[Point] = {
    for {
      x <- 0 until lines.head.length
      y <- lines.indices
      if findRisk(x, y, lines) > 0
    } yield (x, y)
  }

  def basinSize(point: Point, lines: Seq[String]): Int = {
    def accSizer(queue: Queue[Point], vis: Set[Point]): Int = {
      if (queue.isEmpty) {
        vis.size
      } else {
        val(curr, nqueue) =  queue.dequeue
        val(x, y) = curr
        val neighbors = mutable.ListBuffer.empty[Point]
        if (x > 0 && !vis.contains((x - 1, y)) && lines(y).charAt(x - 1) != '9') {
          neighbors += Tuple2(x - 1, y)
        }
        if (x < lines.head.length - 1 && !vis.contains((x + 1, y)) && lines(y).charAt(x + 1) != '9') {
          neighbors += Tuple2(x + 1, y)
        }
        if (y > 0 && !vis.contains((x, y - 1)) && lines(y - 1).charAt(x) != '9') {
          neighbors += Tuple2(x, y - 1)
        }
        if (y < lines.indices.last && !vis.contains((x, y + 1)) && lines(y + 1).charAt(x) != '9') {
          neighbors += Tuple2(x, y + 1)
        }

        accSizer(
          nqueue.enqueueAll(neighbors),
          vis + curr
        )
      }
    }

    accSizer(Queue(point), Set())
  }

  def part2(lines: Seq[String]): Int =
    locateLowPts(lines)
      .map(basinSize(_, lines))
      .sorted
      .reverse
      .take(3)
      .product

}
