package aoc2021

object Day1 {

  def part1(strs: Seq[String]): Int = strs.map(_.toInt).drop(1).foldLeft((0, strs.head.toInt))((cntLast, curr) =>
    if (curr > cntLast._2) (cntLast._1 + 1, curr)
    else (cntLast._1, curr)
  )._1

  def part2(strs: Seq[String]): Int = {
    val nums = strs.map(_.toInt)
    val windows = (0 to (nums.length - 3)).map(i => nums(i) + nums(i + 1) + nums(i + 2))
    windows.drop(1).foldLeft(0, windows.head)((cntLast, curr) =>
      if (curr > cntLast._2) (cntLast._1 + 1, curr)
      else (cntLast._1, curr)
    )._1
  }
}
