package aoc2020

object Day3 {

  /**
   * Determines number of trees hit while traversing the map with the given
   * horizontal (slopeX) and vertical (slopeY) slope components.
   *
   * @param map
   * @param slopeX
   * @param slopeY
   * @return Number of "trees" encountered
   */
  def traverseMountain(map: List[String], slopeX: Int, slopeY: Int): Int = {
    var x, y, tcnt = 0
    while (y < map.length) {
      if (map(y).charAt(x) == '#') {
        tcnt += 1
      }
      y += slopeY
      x = (x + slopeX) % map.head.length
    }
    tcnt
  }

  def part1(strs: List[String]): Int = {
    traverseMountain(strs, 3, 1)
  }

  def part2(strs: List[String]): Long = {
    val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    slopes.map(s => traverseMountain(strs, s._1, s._2)).map(i => i.toLong).product
  }
}
