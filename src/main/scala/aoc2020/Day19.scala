package aoc2020

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day19 {
  trait Enforceable {
    def id: Int
    def isSatisfied(s: String, allRules: Map[Int, Enforceable]): (Boolean, Int)
  }

  class CharRule(_id: Int, _char: Char) extends Enforceable {
    def id: Int = _id

    def isSatisfied(s: String, allRules: Map[Int, Enforceable]): (Boolean, Int) = {
      ((s.nonEmpty && s.charAt(0) == _char), 1)
    }

    override def toString: String = s"CharRule{id:$id}"
  }

  class CompositeRule(_id: Int) extends Enforceable {
    var compositeOpts: Set[List[Int]] = Set.empty[List[Int]]

    def id: Int = _id

    def isOptSatisfied(compOpt: List[Int], s: String, allRules: Map[Int, Enforceable]): (Boolean, Int) = {
      var i = 0;
      var satisfied = true
      for (rid <- compOpt) {
        if (i >= s.length) {
          satisfied = false
        } else {
          val (res, cnt) = allRules(rid).isSatisfied(s.substring(i), allRules)
          if (!res) satisfied = res
          i += cnt
        }
      }
      (satisfied, i)
    }

    def isSatisfied(s: String, allRules: Map[Int, Enforceable]): (Boolean, Int) = {
      println(s"Checking $id : $s")
      val results = compositeOpts.map(isOptSatisfied(_, s, allRules))
      val mr = results.find(res => res._1).getOrElse(results.head)
      println(s"Check rule:: $id : $s => ${mr} (${s.substring(0, Math.min(s.length, mr._2))})")
      results.find(res => res._1).getOrElse(results.head)
    }

    override def toString: String = s"CompositeRule{id:$id}"
  }

  object Rule {
    def parseCompositeParts(parts: Array[String]): Set[List[Int]] = {
      var allParts = mutable.Set.empty[List[Int]]
      var cpart = ListBuffer.empty[Int]
      for (i <- parts.indices) {
        if (parts(i) == "|") {
          allParts += cpart.toList
          cpart = ListBuffer.empty[Int]
        } else {
          cpart += Integer.parseInt(parts(i))
        }
      }
      allParts += cpart.toList
      allParts.toSet
    }

    def fromDescription(s: String): Enforceable = {
      val Array(idStr, ruleDesc) = s.split(':')
      val ruleDescParts = ruleDesc.split(' ').drop(1)
      if (ruleDescParts(0).charAt(0) == '"') {
        new CharRule(Integer.parseInt(idStr), ruleDescParts(0).charAt(1))
      } else {
        val rule = new CompositeRule(Integer.parseInt(idStr))
        rule.compositeOpts = parseCompositeParts(ruleDescParts)
        rule
      }
    }
  }

  def part1(instrs: List[String]): Int = {
    val partitionIndex = instrs.indexWhere(_.trim.isEmpty)
    val (ruleStrs, testStrs) = instrs.splitAt(partitionIndex)
    val rules = ruleStrs.map(Rule.fromDescription).foldLeft(Map.empty[Int, Enforceable])((m, r) => m + (r.id -> r))
    testStrs.drop(1).count((s: String) => {
      val (res, cnt) = rules(0).isSatisfied(s, rules)
      res && cnt == s.length
    })
  }

  def part2(instrs: List[String]): Int = {
    val partitionIndex = instrs.indexWhere(_.trim.isEmpty)
    val (ruleStrs, testStrs) = instrs.splitAt(partitionIndex)
    var rules = ruleStrs.map(Rule.fromDescription).foldLeft(Map.empty[Int, Enforceable])((m, r) => m + (r.id -> r))
    rules += (8 -> Rule.fromDescription("8: 42 | 42 8"))
    rules += (11 -> Rule.fromDescription("11: 42 31 | 42 11 31"))
    val res = testStrs.drop(1).filter((s: String) => {
      val (res, cnt) = rules(0).isSatisfied(s, rules)
      res && cnt == s.length
    })
    println(res)
    res.length
  }
}
