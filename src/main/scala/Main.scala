import aoc2020.{Day3, Day4, Day5, Day6, Day7}

object Main extends App {
  //println(Day4.hasRepeatDigit(51202))
  val instrs = Util.readInputLines("resources/2020/d7.txt")
  println(Day7.part2(instrs))
  //println(Day4.digitFreqs(1122333355).values.find(x => x == 2))
}
