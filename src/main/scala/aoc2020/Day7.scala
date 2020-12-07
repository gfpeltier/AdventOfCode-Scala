package aoc2020

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Bag(val descriptor: String, val color: String) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case obj: Bag =>
        obj.descriptor == this.descriptor && obj.color == this.color
      case _ => false
    }
  }

  override def hashCode(): Int = (descriptor + color).hashCode

  override def toString: String = {
    s"Bag{descriptor=$descriptor, color=$color}"
  }
}

class Rule(val container: Bag, val holdings: Map[Bag, Int]) {
  override def toString: String = {
    s"Rule{container=$container, holdings=$holdings}"
  }
}

object Rule {

  // Example String:
  // "dull bronze bags contain 2 plaid aqua bags, 4 shiny magenta bags, 2 faded green bags, 3 dotted gold bags."
  def fromString(s: String): Rule = {
    val container = raw"(\w+) (\w+) bags contain".r
    val containedBag = raw"(\d+) (\w+) (\w+)".r
    val containerBag = container.findFirstMatchIn(s) match {
      case Some(m) => new Bag(m.group(1), m.group(2))
    }
    var m = mutable.Map.empty[Bag, Int]
    for (cbm <- containedBag.findAllMatchIn(s)) {
      m += (new Bag(cbm.group(2), cbm.group(3)) -> Integer.parseInt(cbm.group(1)))
    }
    new Rule(containerBag, m.toMap)
  }
}

object Day7 {

  def findContainers(ruleTree: List[Rule], childBags: mutable.Set[Bag]): Set[Bag] = {
    var containers = mutable.Set.empty[Bag]
    for (rule <- ruleTree) {
      if (!childBags.contains(rule.container) && rule.holdings.keySet.intersect(childBags).nonEmpty) {
        containers += rule.container
      }
    }
    containers.toSet
  }

  def part1(instrs: List[String]): Int = {
    val rules = instrs.map(Rule.fromString)
    var moved = findContainers(rules, mutable.Set(new Bag("shiny", "gold")))
    var bags = mutable.Set.empty[Bag]
    while (moved.nonEmpty) {
      bags = bags.union(moved)
      moved = findContainers(rules, bags)
    }
    bags.size
  }

  def ruleListToMap(rules: List[Rule]): Map[Bag, Map[Bag, Int]] = {
    rules.foldLeft(mutable.Map.empty[Bag, Map[Bag, Int]])((m, r) => m += (r.container -> r.holdings)).toMap
  }

  def getTotalContainedCount(ruleTree: Map[Bag, Map[Bag, Int]], root: Bag): Int = {
    var count = 0
    var vis = mutable.Set.empty[Bag]
    var q = ListBuffer((root, 1))
    while (q.nonEmpty) {
      var (curr, mult) = q.head
      for (child <- ruleTree(curr)) {
        q += Tuple2(child._1, mult * child._2)
        count += (child._2 * mult)
      }
      vis += curr
      q = q.drop(1)
    }
    count
  }

  def part2(instrs: List[String]): Int = {
    val rootBag = new Bag("shiny", "gold")
    val ruleTree = ruleListToMap(instrs.map(Rule.fromString))
    getTotalContainedCount(ruleTree, rootBag)
  }
}
