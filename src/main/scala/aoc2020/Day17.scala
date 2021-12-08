package aoc2020

import scala.collection.mutable.ListBuffer

// TODO: revisit this later to clean up. Just don't feel like it at the moment.

object Day17 {
  class ConwayCube {
    var state: Array[Array[Array[Char]]] = Array.ofDim[Char](1,1,1)
    var cycCnt = 1

    def runCycle: Unit = {
      cycCnt += 1
      val nlen = state.length + 2
      val nstate = Array.ofDim[Char](nlen, nlen, nlen)
      for (x <- nstate.indices;
           y <- nstate.indices;
           z <- nstate.indices) {
        if (x == 0 || y == 0 || z == 0 ||
            x == nstate.indices.last || y == nstate.indices.last || z == nstate.indices.last) {
          nstate(x)(y)(z) = '.'
        } else {
          nstate(x)(y)(z) = state(x-1)(y-1)(z-1)
        }
      }
      val nstateStart = nstate.map(_.map(_.clone))
      for(x <- nstate.indices;
          y <- nstate.indices;
          z <- nstate.indices) {
        nstate(x)(y)(z) match {
          case '.' => nstate(x)(y)(z) = if (countActiveNeighbors(nstateStart, x, y, z) == 3) '#' else '.'
          case '#' => {
            val ans = countActiveNeighbors(nstateStart, x, y, z)
            nstate(x)(y)(z) = if (ans == 2 || ans == 3) '#' else '.'
          }
        }
        //println(s"($x, $y, $z): ${countActiveNeighbors(nstateStart, x, y, z)} :: ${nstateStart(x)(y)(z)} => ${nstate(x)(y)(z)}")
      }
      state = nstate
      //println(this)
    }

    def countActiveNeighbors(arr: Array[Array[Array[Char]]], x: Int, y: Int, z: Int): Int = {
      var cnt = 0
      for (nx <- Math.max(x-1, 0) to Math.min(x+1, arr.length-1);
           ny <- Math.max(y-1, 0) to Math.min(y+1, arr.length-1);
           nz <- Math.max(z-1, 0) to Math.min(z+1, arr.length-1)
           if (nx != x || ny != y || nz != z)) {
        if (arr(nx)(ny)(nz).equals('#')) cnt += 1
      }
      cnt
    }

    def countActiveCubes: Int = {
      state.flatten.flatten.count(_ == '#')
    }

    override def toString: String = {
      var ostrs = ListBuffer.empty[String]
      var currZ = 0
      val surf = Array.ofDim[Char](state.length, state.length)
      for (z <- state.indices;
           x <- state.indices;
           y <- state.indices) {
        if (currZ != z) {
          ostrs += s"\nz = $currZ"
          ostrs += surf.map(_.mkString).mkString("\n")
          currZ = z
        }
        surf(x)(y) = state(x)(y)(z)
      }
      ostrs += s"\nz = $currZ"
      ostrs += surf.map(_.mkString).mkString("\n")
      ostrs.mkString("\n")
    }
  }

  object ConwayCube {
    def fromInitState(initState: List[String]): ConwayCube = {
      println(initState)
      val cc = new ConwayCube()
      cc.state = Array.ofDim[Char](initState.length, initState.length, initState.length)
      for (x <- initState.indices;
           y <- initState.indices;
           z <- initState.indices) {
        if (z == (initState.length/2)) {
          cc.state(x)(y)(z) = initState(x).charAt(y)
        } else {
          cc.state(x)(y)(z) = '.'
        }
      }
      println(cc)
      cc
    }
  }

  class ConwayCube4D {
    var state: Array[Array[Array[Array[Char]]]] = Array.ofDim[Char](1,1,1, 1)
    var cycCnt = 1

    def runCycle: Unit = {
      cycCnt += 1
      val nlen = state.length + 2
      val nstate = Array.ofDim[Char](nlen, nlen, nlen, nlen)
      for (x <- nstate.indices;
           y <- nstate.indices;
           z <- nstate.indices;
           w <- nstate.indices) {
        if (x == 0 || y == 0 || z == 0 || w == 0 ||
          x == nstate.indices.last || y == nstate.indices.last || z == nstate.indices.last || w == nstate.indices.last) {
          nstate(x)(y)(z)(w) = '.'
        } else {
          nstate(x)(y)(z)(w) = state(x-1)(y-1)(z-1)(w-1)
        }
      }
      val nstateStart = nstate.map(_.map(_.map(_.clone)))
      for(x <- nstate.indices;
          y <- nstate.indices;
          z <- nstate.indices;
          w <- nstate.indices) {
        nstate(x)(y)(z)(w) match {
          case '.' => nstate(x)(y)(z)(w) = if (countActiveNeighbors(nstateStart, x, y, z, w) == 3) '#' else '.'
          case '#' => {
            val ans = countActiveNeighbors(nstateStart, x, y, z, w)
            nstate(x)(y)(z)(w) = if (ans == 2 || ans == 3) '#' else '.'
          }
        }
      }
      state = nstate
    }

    def countActiveNeighbors(arr: Array[Array[Array[Array[Char]]]], x: Int, y: Int, z: Int, w: Int): Int = {
      var cnt = 0
      for (nx <- Math.max(x-1, 0) to Math.min(x+1, arr.length-1);
           ny <- Math.max(y-1, 0) to Math.min(y+1, arr.length-1);
           nz <- Math.max(z-1, 0) to Math.min(z+1, arr.length-1);
           nw <- Math.max(w-1, 0) to Math.min(w+1, arr.length-1)
           if (nx != x || ny != y || nz != z || nw != w)) {
        if (arr(nx)(ny)(nz)(nw).equals('#')) cnt += 1
      }
      cnt
    }

    def countActiveCubes: Int = {
      state.flatten.flatten.flatten.count(_ == '#')
    }
  }

  object ConwayCube4D {
    def fromInitState(initState: List[String]): ConwayCube4D = {
      println(initState)
      val cc = new ConwayCube4D()
      cc.state = Array.ofDim[Char](initState.length, initState.length, initState.length, initState.length)
      for (x <- initState.indices;
           y <- initState.indices;
           z <- initState.indices;
           w <- initState.indices) {
        if (z == (initState.length/2) && w == (initState.length/2)) {
          cc.state(x)(y)(z)(w) = initState(x).charAt(y)
        } else {
          cc.state(x)(y)(z)(w) = '.'
        }
      }
      cc
    }
  }

  def part1(instrs: List[String]): Int = {
    val cc = ConwayCube.fromInitState(instrs)
    while (cc.cycCnt < 7) cc.runCycle
    cc.countActiveCubes
  }

  def part2(instrs: List[String]): Int = {
    val cc = ConwayCube4D.fromInitState(instrs)
    while (cc.cycCnt < 7) cc.runCycle
    cc.countActiveCubes
  }
}
