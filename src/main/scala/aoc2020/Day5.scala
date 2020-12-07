package aoc2020

class Seat(val row: Int, val col: Int) {
  def id: Int = (row * 8) + col

  def nextSeat: Seat = {
    if (col == 7) {
      new Seat(row+1, 0)
    } else {
      new Seat(row, col+1)
    }
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case obj: Seat =>
        obj.row == this.row && obj.col == this.col
      case _ => false
    }
  }

  override def toString: String = {
    s"Seat{row:$row, col:$col}"
  }
}

object Day5 {

  def parseSeatNumStr(sns: String): Seat = {
    var row = 0
    var col = 0
    for (i <- 6 to 0 by -1) {
      val exp = 6 - i
      if (sns.charAt(i) == 'B') {
        row += Math.pow(2, exp).toInt
      }
    }
    for (i <- 9 to 7 by -1) {
      val exp = 9 - i
      if (sns.charAt(i) == 'R') {
        col += Math.pow(2, exp).toInt
      }
    }
    new Seat(row, col)
  }

  def part1(seatNumStrs: List[String]): Int = {
    seatNumStrs.map(s => parseSeatNumStr(s)).maxBy(s => s.id).id
  }

  def part2(seatNumStrs: List[String]): Int = {
    val seats = seatNumStrs.map(s => parseSeatNumStr(s)).sortBy(s => s.id)
    var ns = seats.head
    var i = 0
    while (ns == seats(i)) {
      ns = ns.nextSeat
      i += 1
    }
    ns.id
  }
}
