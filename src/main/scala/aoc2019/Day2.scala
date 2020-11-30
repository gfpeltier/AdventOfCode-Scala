package aoc2019

object Day2 {
  def findNounVerb(code: Array[Int]): (Int, Int) = {
    val target = 19690720
    for (i <- 0 to 99) {
      for (j <- 0 to 99) {
        code(1) = i
        code(2) = j
        val ncode = IntCode.runProgram(code)
        if (ncode(0) == target) return (i, j)
      }
    }
    (-1, -1)
  }
}
