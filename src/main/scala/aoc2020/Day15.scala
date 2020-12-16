package aoc2020

import scala.collection.mutable

object Day15 {
  class NumGame(var turnCount: Int, var lastNum: Int, var histIdxs: mutable.Map[Int, Int]) {
    def nextNum: Int = {
      var next = 0
      if (histIdxs.contains(lastNum)) next = turnCount - histIdxs(lastNum)
      histIdxs += (lastNum -> turnCount)
      lastNum = next
      turnCount += 1
      next
    }
  }

  object NumGame {
    def fromInitState(s: String): NumGame = {
      val nums = s.split(',').map(_.toInt).toList
      val m = nums
        .slice(0, nums.length - 1)
        .zipWithIndex
        .foldLeft(mutable.Map.empty[Int, Int])((m, ni) => m + (ni._1 -> (ni._2 + 1)))
      new NumGame(nums.size, nums.last, m)
    }
  }

  def part1(s: String): Int = {
    val game = NumGame.fromInitState(s)
    while (game.turnCount < 2019) game.nextNum
    game.nextNum
  }

  def part2(s: String): Int = {
    val game = NumGame.fromInitState(s)
    while (game.turnCount < 29_999_999) game.nextNum
    game.nextNum
  }
}
