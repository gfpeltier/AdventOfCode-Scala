package aoc2019

object Day4 {
  def hasRepeatDigit(i: Int): Boolean = {
    val is = i.toString
    val pattern = raw"(.)\1{1,}".r
    pattern.findFirstIn(is) match {
      case Some(_) => true
      case None => false
    }
  }

  def monoInc(i: Int): Boolean = {
    var j = i
    var d = 9
    while (j > 0 && (j % 10) <= d ) {
      d = j % 10
      j = j / 10
    }
    j == 0
  }

  def validPass(i: Int): Boolean = {
    monoInc(i) && hasRepeatDigit(i)
  }

  def part1(start: Int, end: Int): List[Int] = {
    (start to end).foldLeft(List.empty[Int])((l, i) => {if (validPass(i)) l :+ i else l})
  }

  def digitFreqs(i: Int): Map[Int, Int] = {
    var j = i
    var m = Map.empty[Int, Int]
    while (j > 0) {
      val d = j % 10
      if (m.contains(d))
        m += (d -> (m(d) + 1))
      else
        m += (d -> 1)
      j /= 10
    }
    m
  }

  def hasExactPair(i: Int): Boolean = {
    digitFreqs(i).values.find(x => x == 2) match {
      case Some(s) => true
      case None => false
    }
  }

  def part2(start: Int, end: Int): List[Int] = {
    (start to end).foldLeft(List.empty[Int])((l, i) => {if (monoInc(i) && hasExactPair(i)) l :+ i else l})
  }
}
