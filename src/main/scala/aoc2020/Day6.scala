package aoc2020

import scala.collection.mutable.ListBuffer

object Day6 {

  def parseGroups(strs: List[String]): List[Set[Char]] = {
    var groups = ListBuffer.empty[Set[Char]]
    var group = Set.empty[Char]
    for (s <- strs) {
      if (s.trim.isEmpty) {
        groups += group
        group = Set.empty[Char]
      }
      for (c <- s) {
        group += c
      }
    }
    groups += group
    groups.toList
  }

  def parseGroups2(strs: List[String]): List[Set[Char]] = {
    var groups = ListBuffer.empty[Set[Char]]
    var group = ListBuffer.empty[Set[Char]]
    var person = Set.empty[Char]
    for (s <- strs) {
      if (s.trim.isEmpty) {
        groups += group.reduce((p1, p2) => p1.intersect(p2))
        group = ListBuffer.empty[Set[Char]]
      } else {
        for (c <- s) {
          person += c
        }
        group += person
        person = Set.empty[Char]
      }
    }
    groups += group.reduce((p1, p2) => p1.intersect(p2))
    groups.toList
  }

  def part1(strs: List[String]): Int = {
    parseGroups(strs).map(cs => cs.size).sum
  }

  def part2(strs: List[String]): Int = {
    parseGroups2(strs).map(cs => cs.size).sum
  }
}
