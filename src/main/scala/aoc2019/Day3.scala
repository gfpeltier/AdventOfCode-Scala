package aoc2019

import scala.util.control.Breaks.{break, breakable}

class Point(val x: Int, val y: Int) {
  override def toString: String = s"($x, $y)"

  override def equals(obj: Any): Boolean = {
    obj match {
      case obj: Point => {
        obj.isInstanceOf[Point] && obj.x == x && obj.y == y
      }
      case _ => false
    }
  }

  override def hashCode(): Int = {
    (x + y).hashCode()
  }
}

class PathSegment(val start: Point, val end: Point) {
  def xrange: Range = {
    if (start.x < end.x) start.x to end.x
    else start.x to end.x by -1
  }

  def yrange: Range = {
    if (start.y < end.y) start.y to end.y
    else start.y to end.y by -1
  }

  def length: Int = {
    Math.abs(end.x - start.x) + Math.abs(end.y - start.y)
  }

  def points: List[Point] = {
    var range: scala.collection.immutable.Range = 1 to 2
    if (start.x == end.x) {
      if (start.y < end.y) range = start.y until end.y
      else range = start.y until end.y by -1
      range.foldLeft(List[Point]())((l, i) => { l :+ new Point(start.x, i)})
    } else {
      if (start.x < end.x) range = start.x until end.x
      else range = start.x until end.x by -1
      range.foldLeft(List[Point]())((l, i) => { l :+ new Point(i, start.y)})
    }
  }

  def pointInPath(pt: Point): Boolean = {
    (start.x == pt.x && this.yrange.contains(pt.y)) ||
      (start.y == pt.y && this.xrange.contains(pt.x))
  }

  override def toString: String = s"[${start.toString}, ${end.toString}]"
}

object Day3 {
  def createPathSegment(start: Point, pathStr: String): PathSegment = {
    val pathDist = Integer.parseInt(pathStr.substring(1))
    val end = pathStr(0) match {
      case 'U' => new Point(start.x, start.y + pathDist)
      case 'R' => new Point(start.x + pathDist, start.y)
      case 'D' => new Point(start.x, start.y - pathDist)
      case 'L' => new Point(start.x - pathDist, start.y)
    }
    new PathSegment(start, end)
  }

  def createWirePath(inStr: String): List[PathSegment] = {
    val pparts = inStr.split(",").toList
    val start = new Point(0, 0)
    pparts.foldLeft(List.empty[PathSegment])((lb, s) => {
      if (lb.isEmpty) {
        lb :+ createPathSegment(start, s)
      } else{
        lb :+ createPathSegment(lb.last.end, s)
      }
    })
  }

  def pathPoints(segs: List[PathSegment]): Set[Point] = {
    val muset = segs.foldLeft(scala.collection.mutable.Set.empty[Point])((ps, s) => {
      ps ++= s.points
    })
    println(muset.size)
    Set.empty ++ muset
  }

  def part1(s1: String, s2: String): Int = {
    val wp1 = createWirePath(s1)
    val wp2 = createWirePath(s2)
    var intPts = pathPoints(wp1).intersect(pathPoints(wp2))
    println(intPts)
    intPts -= new Point(0, 0)
    intPts.foldLeft(List.empty[Int])((il, p) => {
      il :+ (Math.abs(p.x) + Math.abs(p.y))
    }).min
  }

  def pathPtDist(path: List[PathSegment], pt: Point): Int = {
    var dist = 0
    breakable {
      for(s <- path) {
        if (s.pointInPath(pt)) {
          if (s.start.x == pt.x) dist += Math.abs(pt.y - s.start.y)
          else dist += Math.abs(pt.x - s.start.x)
          break
        } else {
          dist += s.length
        }
      }
    }
    dist
  }

  def part2(s1: String, s2: String): Int = {
    val wp1 = createWirePath(s1)
    val wp2 = createWirePath(s2)
    var intPts = pathPoints(wp1).intersect(pathPoints(wp2))
    intPts -= new Point(0, 0)
    intPts.foldLeft(List.empty[Int])((il, p) => {
      il :+ (pathPtDist(wp1, p) + pathPtDist(wp2, p))
    }).min
  }
}
