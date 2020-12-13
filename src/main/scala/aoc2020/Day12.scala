package aoc2020

import scala.collection.mutable.ListBuffer

object Day12 {

  class Ship(var position: (Int, Int), var heading: Int) {
    def turnLeft(degrees: Int): Unit = {
      if (heading - degrees < 0) {
        heading = 360 + (heading - degrees)
      } else {
        heading -= degrees
      }
    }

    def turnRight(degrees: Int): Unit = {
      heading = (degrees + heading) % 360
    }

    def moveCardinal(dir: Char, dist: Int): Unit = {
      position = dir match {
        case 'N' => (position._1, position._2 + dist)
        case 'S' => (position._1, position._2 - dist)
        case 'E' => (position._1 + dist, position._2)
        case 'W' => (position._1 - dist, position._2)
      }
    }

    def moveForward(dist: Int): Unit = {
      position = (
        position._1 + Math.cos(Math.toRadians(heading)).toInt * dist,
        position._2 + Math.sin(Math.toRadians(heading)).toInt * dist * -1
      )
    }
  }


  class WaypointShip(var pos: (Int, Int), var wpPos: (Int, Int)) {
    def turnLeft(degrees: Int): Unit = {
      val rads = Math.toRadians(degrees)
      wpPos = (
        (wpPos._1 * Math.cos(rads)).toInt - (wpPos._2 * Math.sin(rads)).toInt,
        (wpPos._1 * Math.sin(rads)).toInt + (wpPos._2 * Math.cos(rads)).toInt
      )
    }

    def turnRight(degrees: Int): Unit = {
      val rads = Math.toRadians(-degrees)
      wpPos = (
        (wpPos._1 * Math.cos(rads)).toInt - (wpPos._2 * Math.sin(rads)).toInt,
        (wpPos._1 * Math.sin(rads)).toInt + (wpPos._2 * Math.cos(rads)).toInt
      )
    }

    def moveCardinal(dir: Char, dist: Int): Unit = {
      wpPos = dir match {
        case 'N' => (wpPos._1, wpPos._2 + dist)
        case 'S' => (wpPos._1, wpPos._2 - dist)
        case 'E' => (wpPos._1 + dist, wpPos._2)
        case 'W' => (wpPos._1 - dist, wpPos._2)
      }
    }

    def moveForward(dist: Int): Unit = {
      (1 to dist).foreach(_ => pos = (pos._1 + wpPos._1, pos._2 + wpPos._2))
    }

    override def toString: String = {
      s"WPShip{pos:$pos, wpPos:$wpPos}"
    }
  }

  def parseDirections(instrs: List[String]): List[(Char, Int)] = {
    val dirRegex = raw"([NSEWLRF])(\d+)".r
    instrs.foldLeft(ListBuffer.empty[(Char, Int)])((lb, s) => {
      val t = s match {
        case dirRegex(dir, dist) => (dir(0), dist.toInt)
      }
      lb += t
    }).toList
  }

  def part1(instrs: List[String]): Int = {
    val directions = parseDirections(instrs)
    val ship = new Ship((0, 0), 0)
    directions.foreach(t => {
      t._1 match {
        case 'N' | 'E' | 'S' | 'W' => ship.moveCardinal(t._1, t._2)
        case 'L' => ship.turnLeft(t._2)
        case 'R' => ship.turnRight(t._2)
        case 'F' => ship.moveForward(t._2)
      }
    })
    println(ship.position)
    Math.abs(ship.position._1) + Math.abs(ship.position._2)
  }

  def part2(instrs: List[String]): Int = {
    val directions = parseDirections(instrs)
    val ship = new WaypointShip((0, 0), (10, 1))
    directions.foreach(t => {
      t._1 match {
        case 'N' | 'E' | 'S' | 'W' => ship.moveCardinal(t._1, t._2)
        case 'L' => ship.turnLeft(t._2)
        case 'R' => ship.turnRight(t._2)
        case 'F' => ship.moveForward(t._2)
      }
    })
    println(ship.pos)
    Math.abs(ship.pos._1) + Math.abs(ship.pos._2)
  }
}
