package aoc2020

object Day11 {

  object SeatStatus extends Enumeration {
    type SeatStatus = Value
    val Empty, Floor, Occupied = Value
  }

  class SeatLayout(val layout: Array[Array[SeatStatus.Value]]) {

    def nextSeatStatus(row: Int, col: Int): SeatStatus.Value = {
      var occCnt = 0
      for (
        r <- Math.max(0, row - 1) to Math.min(layout.length - 1, row + 1);
        c <- Math.max(0, col - 1) to Math.min(layout.head.length - 1, col + 1)
        if (r != row || c != col)
      ) if (layout(r)(c) == SeatStatus.Occupied) occCnt += 1
      if (layout(row)(col) == SeatStatus.Empty && occCnt == 0) {
        SeatStatus.Occupied
      } else if (layout(row)(col) == SeatStatus.Occupied && occCnt >= 4) {
        SeatStatus.Empty
      } else {
        layout(row)(col)
      }
    }

    def hasOccOnSlope(row: Int, col: Int, slope: (Int, Int)): Boolean = {
      var r = row + slope._1
      var c = col + slope._2
      while (
        layout.indices.contains(r)
          && layout.head.indices.contains(c)
          && layout(r)(c) == SeatStatus.Floor
      ) {
        r += slope._1
        c += slope._2
      }
      layout.indices.contains(r) && layout.head.indices.contains(c) && layout(r)(c) == SeatStatus.Occupied
    }

    def nextSeatStatus2(row: Int, col: Int): SeatStatus.Value = {
      var occCnt = 0
      val slopes = List(
        (0, 1),
        (1, 1),
        (1, 0),
        (1, -1),
        (0, -1),
        (-1, -1),
        (-1, 0),
        (-1, 1)
      )
      occCnt = slopes.map(hasOccOnSlope(row, col, _)).count(bool => bool)
      if (layout(row)(col) == SeatStatus.Empty && occCnt == 0) {
        SeatStatus.Occupied
      } else if (layout(row)(col) == SeatStatus.Occupied && occCnt >= 5) {
        SeatStatus.Empty
      } else {
        layout(row)(col)
      }
    }

    def nextTick: (SeatLayout, Int) = {
      val nextLayout = layout.map(_.clone)
      var deltaCnt = 0
      for (
        r <- nextLayout.indices;
        c <- nextLayout.head.indices
      ) {
        val nstatus = nextSeatStatus(r, c)
        if (nstatus != layout(r)(c)) {
          deltaCnt += 1
        }
        nextLayout(r)(c) = nstatus
      }
      (new SeatLayout(nextLayout), deltaCnt)
    }

    def nextTick2: (SeatLayout, Int) = {
      val nextLayout = layout.map(_.clone)
      var deltaCnt = 0
      for (
        r <- nextLayout.indices;
        c <- nextLayout.head.indices
      ) {
        val nstatus = nextSeatStatus2(r, c)
        if (nstatus != layout(r)(c)) {
          deltaCnt += 1
        }
        nextLayout(r)(c) = nstatus
      }
      (new SeatLayout(nextLayout), deltaCnt)
    }

    def countOccupied: Int = {
      layout.map(_.count(SeatStatus.Occupied.==)).sum
    }

    override def toString: String = {
      layout.map(_.map {
        case SeatStatus.Floor => '.'
        case SeatStatus.Occupied => '#'
        case SeatStatus.Empty => 'L'
      }.mkString).mkString("\n")
    }
  }

  object SeatLayout {
    def parseLayout(instrs: List[String]): SeatLayout = {
      val arr = Array.ofDim[SeatStatus.Value](instrs.length, instrs.head.length)
      for (
        r <- instrs.indices;
        c <- instrs.head.indices
      ) arr(r)(c) = instrs(r).charAt(c) match {
        case 'L' => SeatStatus.Empty
        case '#' => SeatStatus.Occupied
        case '.' => SeatStatus.Floor
      }
      new SeatLayout(arr)
    }
  }

  def part1(instrs: List[String]): Int = {
    var layout = SeatLayout.parseLayout(instrs)
    var deltaCnt = 1
    var iterCnt = 0
    while (deltaCnt > 0) {
      println(s"NEW LAYOUT: $iterCnt")
      iterCnt += 1
      val (nlayout, ndCnt) = layout.nextTick
      deltaCnt = ndCnt
      println(nlayout)
      println(deltaCnt)
      layout = nlayout
    }
    layout.countOccupied
  }

  def part2(instrs: List[String]): Int = {
    var layout = SeatLayout.parseLayout(instrs)
    var deltaCnt = 1
    var iterCnt = 0
    while (deltaCnt > 0) {
      println(s"NEW LAYOUT: $iterCnt")
      iterCnt += 1
      val (nlayout, ndCnt) = layout.nextTick2
      deltaCnt = ndCnt
      println(nlayout)
      println(deltaCnt)
      layout = nlayout
    }
    layout.countOccupied
  }
}
