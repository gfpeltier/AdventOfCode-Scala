package aoc2020

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

object Day9 {

  // TODO: what is a nicer way to do this without breaks?
  def isNumValid(nums: List[Long], idx: Int): Boolean = {
    if (idx < 25) true
    else {
      var s = mutable.Set.empty[Long]
      var res = false
      breakable {
        for (i <- (idx - 25) until idx) {
          if (s.contains(nums(i))) {
            res = true
            break
          }
          s += (nums(idx) - nums(i))
        }
      }
      res
    }
  }

  def part1(instrs: List[String]): Long = {
    val nums = instrs.map(_.toLong)
    nums.zipWithIndex.find(ni => !isNumValid(nums, ni._2)).get._1
  }

  def part2(instrs: List[String]): Long = {
    val target: Long = 217430975
    val nums = instrs.map(_.toLong)
    var subl = ListBuffer.empty[Long]
    var i = nums.size - 1
    breakable {
      while (i >= 0) {
        if ((nums(i) + subl.sum) <= target) {
          subl += nums(i)
        } else if (nums(i) + subl.drop(1).sum <= target) {
          subl = subl.drop(1) += nums(i)
        } else {
          subl = ListBuffer(nums(i))
        }
        if (subl.size > 1 && subl.sum == target) break
        i -= 1
      }
    }
    subl.min + subl.max
  }
}
