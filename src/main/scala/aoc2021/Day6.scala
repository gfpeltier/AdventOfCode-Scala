package aoc2021

import scala.util.chaining._

object Day6 {

  def dayTick(fish: Seq[Int]): Seq[Int] =
    fish.foldLeft(Seq.empty[Int])((ol, f) =>
      if (f == 0) {
        ol :+ 6 :+ 8
      } else {
        ol :+ f - 1
      }
    )

  def part1(lines: Seq[String]): Int = {
    val totals = (1 to 7).map(i =>
      (1 to 80).foldLeft(Seq(i))((fs, _) => dayTick(fs))
    ).map(_.length)

    lines.head.split(',').map(_.toInt).toSeq
      .pipe(fish =>
        fish.foldLeft(0)((sum, f) => sum + totals(f - 1))
      )
  }

  def part2(lines: Seq[String]): Long = {
    lines.head.split(',').map(_.toInt).toSeq
      .pipe(fish =>
        fish.foldLeft(Seq.fill[Long](9)(0))((arr, f) => arr.updated(f, arr(f) + 1))
      )
      .pipe(spop => (0 until 256).foldLeft(spop)((pop, day) => {
        val cidx = day % 9
        val midx = (day + 7) % 9
        val nidx = (day + 9) % 9
        val upcnt = pop(cidx)
        pop
          .updated(midx, pop(midx) + upcnt)
          .pipe(p2 =>
            p2.updated(nidx, p2(nidx) + upcnt)
              .pipe(p3 =>
                p3.updated(cidx, Math.max(0, p3(cidx) - upcnt))))
      }))
      .foldLeft(0L)(_ + _)
  }
}
