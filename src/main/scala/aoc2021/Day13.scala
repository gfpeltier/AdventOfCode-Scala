package aoc2021

object Day13 {

  type DotCoords = (Int, Int)

  case class Paper(dots: Set[DotCoords])

  object Paper {
    def fromCoordStrings(lines: Seq[String]): Paper =
      Paper(lines.map(s => s.split(',')).map {case Array(x, y) => (x.toInt, y.toInt)}.toSet)

    def foldVert(paper: Paper, x: Int): Paper =
      paper.copy(paper.dots.map(dot =>
        if (dot._1 > x) {
          (x - (dot._1 - x), dot._2)
        } else
          dot)
      )

    def foldHoriz(paper: Paper, y: Int): Paper =
      paper.copy(paper.dots.map(dot =>
        if (dot._2 > y) {
          (dot._1, y - (dot._2 - y))
        } else
          dot)
      )

    def render(paper: Paper): Unit = {
      val maxX = paper.dots.map(_._1).max
      val maxY = paper.dots.map(_._2).max
      (0 to maxY).map(y => (0 to maxX).map(x => if (paper.dots.contains((x, y))) '#' else '.').mkString)
        .foreach(println(_))
    }
  }

  val foldInstr = raw"fold along ([xy])=([0-9]+)".r

  def part1(lines: Seq[String]): Int = {
    val (dotCoords, foldInstrs) = lines.toList.span(_.nonEmpty)
    val paper = Paper.fromCoordStrings(dotCoords)

    foldInstrs.tail.head match {
      case foldInstr("x", xStr) => Paper.foldVert(paper, xStr.toInt).dots.size
      case foldInstr("y", yStr) => Paper.foldHoriz(paper, yStr.toInt).dots.size
    }
  }

  def part2(lines: Seq[String]): Unit = {
    val (dotCoords, foldInstrs) = lines.toList.span(_.nonEmpty)
    val paper = Paper.fromCoordStrings(dotCoords)

    val finalPaper = foldInstrs.tail.foldLeft(paper)((p, instr) => instr match {
      case foldInstr("x", xStr) => Paper.foldVert(p, xStr.toInt)
      case foldInstr("y", yStr) => Paper.foldHoriz(p, yStr.toInt)
    })

    Paper.render(finalPaper)
  }

}
