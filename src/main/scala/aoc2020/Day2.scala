package aoc2020

class ConstrictedPassword(min: Int, max: Int, char: Char, pass: String) {
  def isValid1(): Boolean = {
    var cnt = 0
    for (i <- 0 until pass.length) {
      if (pass.charAt(i) == char)
        cnt += 1
    }
    min to max contains cnt
  }

  def isValid2(): Boolean = {
    (pass.charAt(min-1) == char && pass.charAt(max-1) != char) ||
      (pass.charAt(min-1) != char && pass.charAt(max-1) == char)
  }
}

object Day2 {
  def toConstrictedPass(s: String): ConstrictedPassword = {
    val regex = raw"(\d+)-(\d+) ([a-z]): ([a-z]+)".r
    s match {
      case regex(min, max, char, pass) =>
        new ConstrictedPassword(Integer.parseInt(min), Integer.parseInt(max), char.charAt(0), pass)
      case _ => throw new IllegalArgumentException
    }
  }

  def part1(strs: List[String]): Int = {
    val cpasses = strs.map(s => toConstrictedPass(s))
    cpasses.count(p => p.isValid1())
  }

  def part2(strs: List[String]): Int = {
    val cpasses = strs.map(s => toConstrictedPass(s))
    cpasses.count(p => p.isValid2())
  }
}
