package aoc2021

import scala.util.chaining._

object Day11 {

  type OctGrid = Seq[Seq[Int]]

  def parseGrid(lines: Seq[String]): OctGrid =
    lines.map(line => line.map(c => c.toInt - 0x30))

  def flash(grid: OctGrid, x: Int, y: Int): OctGrid =
    (for {
      ix <- Math.max(x - 1, 0) to Math.min(x + 1, grid.head.length - 1)
      iy <- Math.max(y - 1, 0) to Math.min(y + 1, grid.length - 1)
    } yield (ix, iy)).foldLeft(grid)((g, pt) => {
      val(ix, iy) = pt
      if (ix == x && iy == y)
        g.updated(iy, g(iy).updated(ix, 0))
      else if (g(iy)(ix) != 0)
        g.updated(iy, g(iy).updated(ix, g(iy)(ix) + 1))
      else g
    })

  def countZeros(grid: OctGrid): Int =
    grid.foldLeft(0)((sum, row) => sum + row.foldLeft(0)((s, i) => if (i == 0) s + 1 else s))

  def tick(grid: OctGrid): OctGrid = {
    var nflashes = 0
    var ngrid: OctGrid = grid.map(row => row.map(_ + 1))
    do {
      nflashes = 0
      val lgrid = ngrid
      ngrid = (for {
        x <- ngrid.head.indices
        y <- ngrid.indices
      } yield (x, y)).foldLeft(ngrid)((g, pt) => {
        val(x, y) = pt
        if(g(y)(x) > 9) flash(g, x, y) else g
      })
      nflashes += countZeros(ngrid) - countZeros(lgrid)
    } while (nflashes > 0)
    ngrid
  }

  def printGrid(grid: OctGrid): Unit =
    grid.foreach(println(_))

  def part1(lines: Seq[String]): Int =
    parseGrid(lines).pipe(og => (1 to 100).foldLeft((og, 0))((gsum, i) => {
      val ng = tick(gsum._1)
      (ng, gsum._2 + countZeros(ng))
    }))._2

  def part2(lines: Seq[String]): Int = {
    var grid = parseGrid(lines)
    var i = 0
    while(countZeros(grid) < 100) {
      i += 1
      grid = tick(grid)
    }
    i
  }
}
