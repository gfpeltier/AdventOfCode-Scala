package aoc2020

import scala.util.control.Breaks._

// TODO: What is a nicer way to do this without breaks...

object Day1 {
  def part1(is: List[String]): Int = {
    val nums = is.map(s => Integer.parseInt(s))
    var sol = 0
    breakable{
      for (i <- nums.indices;
           j <- nums.indices
           if (i < j)) {
        if (nums(j) + nums(i) == 2020) {
          sol = nums(i) * nums(j)
          break
        }
      }
    }
    sol
  }

  def part2(is: List[String]): Int = {
    val nums = is.map(s => Integer.parseInt(s))
    var i = 0
    var sol = -1
    breakable {
      for (
        i <- nums.indices;
        j <- nums.indices;
        k <- nums.indices
        if (i < j && j < k)
      ) {
        if (nums(i) + nums(j) + nums(k) == 2020) {
          sol = nums(i) * nums(j) * nums(k)
          break
        }
      }
    }
    sol
  }
}
