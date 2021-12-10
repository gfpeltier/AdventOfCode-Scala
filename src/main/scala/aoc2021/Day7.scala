package aoc2021

object Day7 {

  def part1(lines: Seq[String]): Int = {
    val nums = lines.head.split(',').map(_.toInt).sorted
    val med = nums.drop(nums.length/2).head
    nums.foldLeft(0)((sum, cx) => sum + Math.abs(cx - med))
  }

  def part2(lines: Seq[String]): Int = {
    val nums = lines.head.split(',').map(_.toInt)
    val avg = nums.sum / nums.length
    nums.foldLeft(0)((sum, cx) => {
      val n = Math.abs(cx - avg)
      val cost = (n * (n + 1)) / 2
      sum + cost
    })
  }

}
