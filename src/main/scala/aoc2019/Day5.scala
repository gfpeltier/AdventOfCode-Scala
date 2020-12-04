package aoc2019

object Day5 {
  def part1(progStr: String) = {
    val prog = IntCode.parseCode(progStr)
    IntCode.runProgram(prog)
  }
}
