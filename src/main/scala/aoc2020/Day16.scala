package aoc2020

object Day16 {
  case class Ticket(fields: List[Int])

  case class Rule(key: String, validRanges: Set[Range]) {
    def isSatisfied(value: Int): Boolean = {
      validRanges.exists(_.contains(value))
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case obj: Rule => this.key == obj.key
        case _ => false
      }
    }

    override def hashCode(): Int = {
      this.key.hashCode()
    }

    override def toString: String = {
      s"Rule{key:$key}"
    }
  }

  def strToRange(s: String): Range = {
    val is = s.split("-")
    is(0).trim.toInt to is(1).trim.toInt
  }

  def parseRules(instrs: List[String]): List[Rule] = {
    println(instrs)
    instrs.map(s => {
      val ps = s.split(":")
      val rstrs = ps(1).split(" or ")
      Rule(ps(0), Set(strToRange(rstrs(0)), strToRange(rstrs(1))))
    })
  }

  def partitionInput(instrs: List[String]): (List[String], List[String], List[String]) = {
    val listArr = Array(List.empty[String], List.empty[String], List.empty[String])
    var l = 0
    for (i <- instrs.indices) {
      if (instrs(i).trim.isBlank) {
        l += 1
      } else {
        listArr(l) = listArr(l) :+ instrs(i)
      }
    }
    (listArr(0), listArr(1), listArr(2))
  }

  def part1(instrs: List[String]): Int = {
    val (ruleStrs, myTktStrs, otherTktStrs) = partitionInput(instrs)
    val rules = parseRules(ruleStrs)
    val otherTickets = otherTktStrs.drop(1).map(s => Ticket(s.split(",").map(_.toInt).toList))
    var badFields = List.empty[Int]
    badFields ++= otherTickets.flatMap(t => t.fields.filter(i => !rules.exists(r => r.isSatisfied(i))))
    badFields.sum
  }

  def part2(instrs: List[String]): Long = {
    val (ruleStrs, myTktStrs, otherTktStrs) = partitionInput(instrs)
    val rules = parseRules(ruleStrs)
    val myTicket = Ticket(myTktStrs(1).split(",").map(_.toInt).toList)
    val otherTickets = otherTktStrs.drop(1)
      .map(s => Ticket(s.split(",").map(_.toInt).toList))
      .filter(t => t.fields.forall(i => rules.exists(r => r.isSatisfied(i))))
    val validTickets = otherTickets :+ myTicket
    val fieldPossArr = Array.fill[Set[Rule]](20)(rules.toSet)
    for (t <- validTickets;
         fi <- t.fields.indices;
         r <- rules) {
      if (!r.isSatisfied(t.fields(fi))) {
        fieldPossArr(fi) -= r
      }
    }
    var fieldIndexes = Map.empty[Rule,Int]
    while (fieldIndexes.keySet.size < 20) {
      for (rsi <- fieldPossArr.indices) {
        if (fieldPossArr(rsi).size == 1) {
          val r = fieldPossArr(rsi).head
          println(s"$rsi - $r")
          fieldIndexes += (r -> rsi)
          for (pai <- fieldPossArr.indices
               if pai != rsi) {
            fieldPossArr(pai) -= r
          }
        }
      }
    }
    fieldIndexes.filter(fi => fi._1.key.startsWith("departure")).values
      .map(myTicket.fields(_)).map(_.toLong).product
  }
}
