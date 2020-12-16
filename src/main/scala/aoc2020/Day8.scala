package aoc2020

import com.sun.net.httpserver.Authenticator.Failure

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object Day8 {
  def parseInstructions(instrs: List[String]): List[(String, Int)] = {
    instrs.foldLeft(ListBuffer.empty[(String, Int)])((lb, s) => {
      lb += Tuple2(
        s.split(" ")(0),
        Integer parseInt s.split(" ")(1)
      )
      lb
    }).toList
  }

  def runProgram(instructions: List[(String, Int)], retAccOnRepeat: Boolean): Option[Int] = {
    var acc = 0
    var completedInstrs = mutable.Set.empty[Int]
    var pc = 0
    while (!completedInstrs.contains(pc) && pc < instructions.length) {
      val (nacc, npc) = instructions(pc) match {
        case ("acc", value) => (acc + value, pc + 1)
        case ("jmp", value) => (acc, pc + value)
        case _ => (acc, pc + 1)
      }
      completedInstrs += pc
      acc = nacc
      pc = npc
    }
    if (pc >= instructions.length || retAccOnRepeat) Some(acc)
    else None
  }

  def part1(instrs: List[String]): Int = {
    val instructions = parseInstructions(instrs)
    runProgram(instructions, retAccOnRepeat = true).get
  }

  def swapInstruction(instrs: List[(String, Int)], index: Int): List[(String, Int)] = {
    if (instrs(index)._1 == "jmp") instrs.updated(index, ("nop", 0))
    else instrs.updated(index, ("jmp", instrs(index)._2))
  }

  def part2(instrs: List[String]): Int = {
    val instructions = parseInstructions(instrs)
    var out = 0
    for (i <- instructions.indices) {
      if (Set("jmp", "nop").contains(instructions(i)._1)) {
        out = runProgram(swapInstruction(instructions, i), retAccOnRepeat = false).getOrElse(out)
      }
    }
    out
  }
}
