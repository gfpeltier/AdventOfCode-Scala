package aoc2021

import scala.collection.mutable

object Day14 {

  type InsertRules = Map[(Char, Char), Char]

  def iterateTimes(state: String, rules: InsertRules, iterations: Int): String = {
    def iterate: String =
      (1 until state.length)
        .map(i => List(
          state.charAt(i - 1),
          rules((state.charAt(i - 1), state.charAt(i)))
        ))
        .foldLeft(mutable.ArrayBuffer.empty[Char])((b, chars) => b ++= chars)
        .mkString + state.last

    if (iterations > 0) {
      iterateTimes(iterate, rules, iterations - 1)
    } else {
      state
    }
  }

  def charFreqs(str: String): Map[Char, Long] =
    str.foldLeft(Map.empty[Char, Long])((m, c) => m.updatedWith(c){
      case Some(i) => Some(i + 1)
      case None => Some(1)
    })

  def part1(lines: Seq[String]): Long = {
    val start = lines.head

    val rules: InsertRules = lines
      .drop(2)
      .map(rStr => rStr.split(" -> "))
      .map{case Array(ruleChars, ic) => (ruleChars.charAt(0), ruleChars.charAt(1)) -> ic.charAt(0)}
      .toMap

    val lastStr = iterateTimes(start, rules, 10)
    val freqs = charFreqs(lastStr)
    println(freqs)
    freqs.values.max - freqs.values.min
  }

  type PairState = Map[(Char, Char), Long]
  type ApplicableRules = Map[(Char, Char), List[(Char, Char)]]

  def digestRules(rules: InsertRules): ApplicableRules = rules.map { case (k, v) =>
    k -> List((k._1, v), (v, k._2))
  }

  implicit class AddMaps[K, V](value: Map[K, V]) {
    def mergeWith(other: Map[K, V], f: (V, V) => V): Map[K, V] = (value.toList ++ other.toList).groupBy(_._1).map {
      case (k, vs) => k -> vs.map(_._2).reduce(f)
    }
  }

  def iteratePairsTimes(state: PairState, rules: ApplicableRules, iterations: Int): PairState = {
    def iterate: PairState = state.toList.flatMap {
      case (k, v) => rules(k).map(nk => (nk, v)) :+ (k, -v)
    }.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2).sum }.mergeWith(state, Numeric[Long].plus)

    if (iterations > 0) {
      iteratePairsTimes(iterate, rules, iterations - 1)
    } else {
      state
    }
  }

  def charFreqs(state: PairState): Map[Char, Long] = state.foldLeft(Map.empty[Char, Long]) { case (m, ((_, c2), v)) =>
    m.updatedWith(c2) {
      case Some(v1) => Some(v1 + v)
      case _        => Some(v)
    }
  }

  def part2(lines: Seq[String]): Long = {
    val start = lines.head

    val rules: InsertRules = lines
      .drop(2)
      .map(rStr => rStr.split(" -> "))
      .map{case Array(ruleChars, ic) => (ruleChars.charAt(0), ruleChars.charAt(1)) -> ic.charAt(0)}
      .toMap

    val pairs = (1 until start.length)
      .map(i => List((start.charAt(i - 1), start.charAt(i))))
      .reduce(_ ++ _)
      .foldLeft(Map.empty[(Char, Char), Long])((m, chars) => m.updatedWith(chars) {
        case Some(value) => Some(value + 1)
        case None => Some(1)
      })

    val finalState = iteratePairsTimes(pairs, digestRules(rules), 40)
    val freqs = charFreqs(finalState).updatedWith(start.head){
      case Some(v) => Some(v + 1)
    }

    freqs.values.max - freqs.values.min
  }

}
