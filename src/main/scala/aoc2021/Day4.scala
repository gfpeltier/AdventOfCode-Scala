package aoc2021

object Day4 {

  implicit class SeqSplitOp[T](coll: Seq[T]) {
    def split(pred: T => Boolean): Seq[Seq[T]] =
      coll.foldLeft(Seq(Seq.empty[T]))((grp, t) =>
        if (pred(t))
          grp :+ Seq.empty[T]
        else
          grp.updated(grp.length - 1, grp.last :+ t))
  }

  def parseCard(nrows: Seq[String]): Seq[Seq[Int]] = nrows.map(_.split("\\s+").filter(_.nonEmpty).map(_.toInt))

  def didWin(card: Seq[Seq[Int]], drawnNums: Set[Int]): Boolean = {
    val rowWin = card.find(_.forall(drawnNums.contains))
    val colWin = card.head.indices.find(n => card.forall(row => drawnNums.contains(row(n))))
    rowWin.isDefined || colWin.isDefined
  }

  def scoreCard(card: Seq[Seq[Int]], drawnNums: Set[Int], lastNum: Int): Int = {
    lastNum * card.foldLeft(0)((sum, row) =>
      sum + row.foldLeft(0)((s, n) =>
        if (!drawnNums.contains(n)) s + n else s)
    )
  }

  def part1(lines: Seq[String]): Int = {
    val inParts = lines.split(_.isEmpty)
    val numOrd = inParts.head.head.split(',').map(_.toInt)
    val cards = inParts.tail.map(parseCard)
    var i = 1
    while (!cards.exists(didWin(_, numOrd.take(i).toSet))) {
      i += 1
    }
    cards.find(didWin(_, numOrd.take(i).toSet)) match {
      case Some(card) => scoreCard(card, numOrd.take(i).toSet, numOrd.take(i).last)
      case None => -1
    }
  }

  def part2(lines: Seq[String]): Int = {
    val inParts = lines.split(_.isEmpty)
    val numOrd = inParts.head.head.split(',').map(_.toInt)
    val cards = inParts.tail.map(parseCard)
    var i = 1
    while (cards.filterNot(didWin(_, numOrd.take(i).toSet)).length > 1) i += 1
    val finalCard = cards.filterNot(didWin(_, numOrd.take(i).toSet)).head
    while (!didWin(finalCard, numOrd.take(i).toSet)) i += 1
    scoreCard(finalCard, numOrd.take(i).toSet, numOrd.take(i).last)
  }

}
