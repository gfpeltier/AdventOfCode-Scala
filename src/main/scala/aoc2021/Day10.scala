package aoc2021

import scala.util.chaining.scalaUtilChainingOps

object Day10 {

  def firstIllegal(line: String): Option[Char] =
    line.foldLeft((List.empty[Char], Option.empty[Char]))((agg, c) => {
      val(stack, optC) = agg
      c match {
        case '{' | '(' | '<' | '[' => (c :: stack, optC)
        case '}' => if (stack.head == '{') (stack.tail, optC) else if (optC.isEmpty) (stack, Some(c)) else (stack, optC)
        case ']' => if (stack.head == '[') (stack.tail, optC) else if (optC.isEmpty) (stack, Some(c)) else (stack, optC)
        case '>' => if (stack.head == '<') (stack.tail, optC) else if (optC.isEmpty) (stack, Some(c)) else (stack, optC)
        case ')' => if (stack.head == '(') (stack.tail, optC) else if (optC.isEmpty) (stack, Some(c)) else (stack, optC)
      }
    })._2

  def part1(lines: Seq[String]): Int = {
    val pmap = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
    lines
      .map(firstIllegal)
      .map {
        case Some(c) => pmap.getOrElse(c, 0)
        case None => 0
      }
      .sum
  }

  def completeLine(line: String): String = {
    val cmap = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
    line
      .foldLeft(List.empty[Char])((stack, c) =>
        c match {
          case '{' | '(' | '<' | '[' => c :: stack
          case '}' | ')' | '>' | ']' => stack.tail
        }
      )
      .map(cmap.getOrElse(_, '_'))
      .mkString
  }

  def scoreLine(line: String): Long = {
    val pmap = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
    line.foldLeft(0L)((total, c) => total * 5 + pmap.getOrElse(c, 0))
  }


  def part2(lines: Seq[String]): Long =
    lines
      .filter(firstIllegal(_).isEmpty)
      .map(completeLine)
      .map(scoreLine)
      .sorted
      .pipe(scores => scores(scores.length / 2))

}
