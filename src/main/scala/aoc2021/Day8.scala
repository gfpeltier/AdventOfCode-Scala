package aoc2021

import scala.util.chaining._

object Day8 {

  case class NumDisplayTest(testChars: Seq[Set[Char]], realChars: Seq[Set[Char]]) {
    private def simpleChars: Map[Int, Set[Char]] = testChars.foldLeft(Map.empty[Int, Set[Char]])((m, tc) =>
      tc.size match {
        case 2 => m + (1 -> tc)
        case 3 => m + (7 -> tc)
        case 4 => m + (4 -> tc)
        case 7 => m + (8 -> tc)
        case _ => m
      }
    )

    def solve: Int =
      testChars.foldLeft(simpleChars)((m, tc) =>
        tc.size match {
          // 2/3/5
          case 5 => if (m.getOrElse(1, Set('X')) subsetOf tc) {
            m + (3 -> tc)
          } else if ((m.getOrElse(4, Set('X')) diff tc).size == 2) {
            m + (2 -> tc)
          } else m + (5 -> tc)

          // 0/6/9
          case 6 => if (m.getOrElse(4, Set('X')) subsetOf tc) {
            m + (9 -> tc)
          } else if (m.getOrElse(1, Set('X')) subsetOf tc) {
            m + (0 -> tc)
          } else m + (6 -> tc)

          case _ => m
        }
      ).map(_.swap).pipe(charM =>
        realChars.zipWithIndex.foldLeft(0)((num, charSI) =>
          charM.get(charSI._1) match {
            case Some(n) => num + (n * Math.pow(10, 3 - charSI._2).toInt)
            case None => throw new Exception(s"oops. Failed to solve for: ${charSI._1}")
          }
        )
      )
  }

  object NumDisplayTest {
    def fromString(line: String): NumDisplayTest =
      line.split(" \\| ").pipe(parts =>
        NumDisplayTest(parts.head.split(' ').map(_.toSet), parts.last.split(' ').map(_.toSet))
      )
  }

  def part1(lines: Seq[String]): Int =
    lines.map(NumDisplayTest.fromString).foldLeft(0)((sum, ndt) =>
      sum + ndt.realChars.count(rc =>
        rc.size == 2 || rc.size == 3 || rc.size == 4 || rc.size == 7
      )
    )

  def part2(lines: Seq[String]): Int = lines.map(NumDisplayTest.fromString).map(_.solve).sum

}
