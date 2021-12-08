package aoc2020

import scala.collection.mutable

object Day15 {

  class NumGame(var history: Map[Int, Int], var lastNum: Int, var turnCount: Int) {
    def nextNum: Int = {
      println(s"Last num: $lastNum")
      turnCount += 1
      var next = 0
      if (history.contains(lastNum)) {
        next = turnCount - history(lastNum)
      }
      println(s"Next: $next")
      lastNum = next
      history += (next -> turnCount)
      next
    }
  }

  object NumGame {
    def fromInitState(s: String): NumGame = {
      val nums = s.split(',').map(_.toInt)
      val m = nums.zipWithIndex.foldLeft(Map.empty[Int, Int])((m, ni) => m + (ni._1 -> ni._2))
      new NumGame(m, nums.last, nums.length-1)
    }
  }

  def part1(s: String): Int = {
    val game = NumGame.fromInitState(s)
    while (game.turnCount < 2020) game.nextNum
    println(game.history)
    game.nextNum
  }
}
