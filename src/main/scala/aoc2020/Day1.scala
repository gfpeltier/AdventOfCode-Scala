package aoc2020

import scala.util.control.Breaks._

// TODO: What is a nicer way to do this without breaks...

object Day1 {
  def part1(is: List[String]): Int = {
    val nums = is.map(s => Integer.parseInt(s))
    var i = 0
    var sol = 0
    breakable{
      while (i < nums.length) {
        for (j <- (i + 1) until nums.length) {
          if (nums(j) + nums(i) == 2020) {
            sol = nums(i) * nums(j)
            break
          }
        }
        i += 1
      }
    }
    sol
  }

  def part2(is: List[String]): Int = {
    val nums = is.map(s => Integer.parseInt(s))
    var i = 0
    var sol = -1
    breakable {
      while (i < nums.length) {
        for (j <- (i + 1) until nums.length) {
          for (k <- (j + 1) until nums.length) {
            if (nums(i) + nums(j) + nums(k) == 2020) {
              sol = nums(i) * nums(j) * nums(k)
              break
            }
          }
        }
        i += 1
      }
    }
    sol
  }
}
