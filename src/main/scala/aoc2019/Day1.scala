package aoc2019

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day1 {
  def readInput(): List[Int] = {
    val nums = new ListBuffer[Int]()
    val buffSrc = Source.fromFile("d1.txt")
    for (l <- buffSrc.getLines) {
      nums += Integer.parseInt(l)
    }
    buffSrc.close()
    nums.toList
  }

  def calcModFuelReq(weight: Int): Int = {
    (Math.floor(weight / 3) - 2).toInt
  }

  def calcFuel(weights: List[Int]): Int = {
    var out = 0
    for (n <- weights) out = out + calcModFuelReq(n)
    out
  }

  def calcFuelRecur(weight: Int): Int = {
    println(s"Base weight: ${weight}")
    var totalFW = weight
    var fw = calcModFuelReq(weight)
    while (fw > 0) {
      totalFW = totalFW + fw
      fw = (calcModFuelReq(fw))
    }
    println(s"Total: $totalFW")
    totalFW
  }
}
