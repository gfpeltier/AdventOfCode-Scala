package aoc2021

import scala.util.chaining._

object Day2 {

  sealed trait Direction {
    def move(x: Int, y: Int, delta: Int): (Int, Int)
    def move2(x: Int, y: Int, aim: Int, delta: Int): (Int, Int, Int)
  }

  object Direction {
    def fromString(str: String): Either[Exception, Direction] = str match {
      case "up" => Right(Up)
      case "down" => Right(Down)
      case "forward" => Right(Forward)
      case _ => Left(new IllegalArgumentException(s"$str is not a valid direction"))
    }
  }

  final case object Up extends Direction {
    def move(x: Int, y: Int, delta: Int): (Int, Int) = (x, y - delta)
    def move2(x: Int, y: Int, aim: Int, delta: Int): (Int, Int, Int) = (x, y, aim - delta)
  }

  final case object Down extends Direction {
    def move(x: Int, y: Int, delta: Int): (Int, Int) = (x, y + delta)
    def move2(x: Int, y: Int, aim: Int, delta: Int): (Int, Int, Int) = (x, y, aim + delta)
  }

  final case object Forward extends Direction {
    def move(x: Int, y: Int, delta: Int): (Int, Int) = (x + delta, y)
    def move2(x: Int, y: Int, aim: Int, delta: Int): (Int, Int, Int) = (x + delta, y + (aim * delta), aim)
  }

  case class Instruction(dir: Direction, dist: Int)

  def part1(strs: Seq[String]): Int =
    strs
      .map{s =>
        Direction.fromString(s.split(' ')(0))
          .map(dir => Instruction(dir, s.split(' ')(1).toInt))
      }.foldLeft((0, 0))((tup, errOrInst) =>
      errOrInst match {
        case Right(instr) => instr.dir.move(tup._1, tup._2, instr.dist)
        case _ => tup
      }).pipe{case (x, y) => x * y}

  def part2(strs: Seq[String]): Int =
    strs
      .map{s =>
        Direction.fromString(s.split(' ')(0))
          .map(dir => Instruction(dir, s.split(' ')(1).toInt))
      }.foldLeft((0, 0, 0))((tup, errOrInst) =>
      errOrInst match {
        case Right(instr) => instr.dir.move2(tup._1, tup._2, tup._3, instr.dist)
        case _ => tup
      }).pipe{case (x, y, _) => x * y}

}
