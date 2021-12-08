package aoc2020

object Day20 {

  object TileSide extends Enumeration {
    val N, S, E, W = Value
  }

  class Tile(val id: Int, var matrix: Array[Array[Char]]) {
    def transpose(matrix: Array[Array[Char]]): Array[Array[Char]] = {
      matrix.head.indices.map(i => matrix.map(_(i))).toArray
    }

    def rotate90(): Unit = {
      matrix = transpose(matrix).map(_.reverse)
    }

    def extractSide(side: TileSide.Value): Array[Char] = {
      side match {
        case TileSide.N => matrix.head
        case TileSide.S => matrix.last
        case TileSide.W => matrix.map(_.head)
        case TileSide.E => matrix.map(_.last)
      }
    }

    def sideMatches(other: Tile, thisSide: TileSide.Value, otherSide: TileSide.Value): Boolean = {
      this.extractSide(thisSide) sameElements other.extractSide(otherSide)
    }

    override def toString: String = s"Tile{id: $id\n matrix:\n${matrix.map(_.mkString).mkString("\n")}}"
  }

  object Tile {
    def fromDescription(instrs: List[String]): Tile = {
      val id = Integer.parseInt(instrs.head.split(":").head.split(" ").last)
      val matrix = instrs.drop(1).map(_.toCharArray).toArray
      new Tile(id, matrix)
    }
  }

  class Image(sideLen: Int) {
    var tiles = Array.empty[Array[Tile]]

    def fill(tiles: List[Tile]): Unit = {
      this.tiles = tiles.grouped(sideLen).map(_.toArray).toArray
    }

    def isValid: Boolean = {
      val horizEdgesMatch = tiles.dropRight(1)
        .zipWithIndex
        .flatMap(rowi =>
          rowi._1.zipWithIndex.map(ti => ti._1.sideMatches(tiles(rowi._2+1)(ti._2), TileSide.S, TileSide.N)))
        .forall(_ == true)
      val vertEdgesMatch = tiles
        .zipWithIndex
        .flatMap(rowi =>
          rowi._1.dropRight(1)
            .zipWithIndex
            .map(ti => ti._1.sideMatches(tiles(rowi._2)(ti._2+1), TileSide.E, TileSide.W)))
        .forall(_ == true)
      horizEdgesMatch && vertEdgesMatch
    }

    def permute(): Unit = {
      ???
    }

    def imageId: Long = {
      tiles.head.head.id * tiles.head.last.id * tiles.last.head.id * tiles.last.last.id
    }
  }

  def separateTileDescriptions(instrs: List[String]): List[List[String]] = {
    val blankIndices = instrs.zipWithIndex.filter(_._1.trim.isEmpty).map(_._2)
    blankIndices.zipWithIndex.map(bii => if (bii._2 == 0) {
      instrs.slice(0, bii._1)
    } else {
      instrs.slice(blankIndices(bii._2 - 1) + 1, bii._1)
    })
  }

  def part1(instrs: List[String]): Long = {
    val tiles = separateTileDescriptions(instrs).map(Tile.fromDescription)
    val img = new Image(Math.sqrt(tiles.length).toInt)
    img.fill(tiles)
    while (!img.isValid) img.permute()
    img.imageId
  }

}
