package aoc2019

import scala.io.StdIn.readLine

object IntCode {
  def parseCode(str: String): Array[Int] = {
    str.split(",").map(s => Integer.parseInt(s))
  }

  def evalParam(code: Array[Int], param: Int, mode: Int): Int = {
    mode match {
      case 0 => code(param)
      case 1 => param
    }
  }

  def isolateMode(op: Int, paramIdx: Int): Int = {
    Math.floor(op / Math.pow(10, 2 + paramIdx)).toInt % 10
  }

  def opAdd(code: Array[Int], pc: Int): (Array[Int], Int) = {
    val op = code(pc)
    val op1 = evalParam(code, code(pc + 1), isolateMode(op, 0))
    val op2 = evalParam(code, code(pc + 2), isolateMode(op, 1))
    val outAddr = code(pc + 3)
    code(outAddr) = op1 + op2
    (code, pc+4)
  }

  def opMultiply(code: Array[Int], pc: Int): (Array[Int], Int) = {
    val op = code(pc)
    val op1 = evalParam(code, code(pc + 1), isolateMode(op, 0))
    val op2 = evalParam(code, code(pc + 2), isolateMode(op, 1))
    val outAddr = code(pc + 3)
    code(outAddr) = op1 * op2
    (code, pc+4)
  }

  def opInput(code: Array[Int], pc: Int): (Array[Int], Int) = {
    val op1Addr = code(pc + 1)
    print("Enter input: ")
    val in = readLine()
    code(op1Addr) = Integer.parseInt(in)
    (code, pc+2)
  }

  def opOutput(code: Array[Int], pc: Int): (Array[Int], Int) = {
    val op = code(pc)
    val op1 = evalParam(code, code(pc + 1), isolateMode(op, 0))
    println(s"Output: ${op1}")
    (code, pc+2)
  }

  def opJumpTrue(code: Array[Int], pc: Int): (Array[Int], Int) = {
    val op = code(pc)
    val op1 = evalParam(code, code(pc + 1), isolateMode(op, 0))
    val op2 = evalParam(code, code(pc + 2), isolateMode(op, 1))
    (code, if (op1 != 0) op2 else pc+3)
  }

  def opJumpFalse(code: Array[Int], pc: Int): (Array[Int], Int) = {
    val op = code(pc)
    val op1 = evalParam(code, code(pc + 1), isolateMode(op, 0))
    val op2 = evalParam(code, code(pc + 2), isolateMode(op, 1))
    (code, if (op1 == 0) op2 else pc+3)
  }

  def opLessThan(code: Array[Int], pc: Int): (Array[Int], Int) = {
    val op = code(pc)
    val op1 = evalParam(code, code(pc + 1), isolateMode(op, 0))
    val op2 = evalParam(code, code(pc + 2), isolateMode(op, 1))
    val outAddr = code(pc + 3)
    code(outAddr) = if (op1 < op2) 1 else 0
    (code, pc+4)
  }

  def opEquals(code: Array[Int], pc: Int): (Array[Int], Int) = {
    val op = code(pc)
    val op1 = evalParam(code, code(pc + 1), isolateMode(op, 0))
    val op2 = evalParam(code, code(pc + 2), isolateMode(op, 1))
    val outAddr = code(pc + 3)
    code(outAddr) = if (op1 == op2) 1 else 0
    (code, pc+4)
  }

  def runProgram(code: Array[Int]): Array[Int] = {
    var pc = 0
    var opc = code(pc)
    var ncode = code.clone
    var cnt = 0
    while (opc != 99) {
      cnt += 1
      val(ocode, npc) = opc % 100 match {
        case 1 => opAdd(ncode, pc)
        case 2 => opMultiply(ncode, pc)
        case 3 => opInput(ncode, pc)
        case 4 => opOutput(ncode, pc)
        case 5 => opJumpTrue(ncode, pc)
        case 6 => opJumpFalse(ncode, pc)
        case 7 => opLessThan(ncode, pc)
        case 8 => opEquals(ncode, pc)
        case _ => throw new UnsupportedOperationException
      }
      ncode = ocode
      pc = npc
      opc = ncode(pc)
    }
    ncode
  }
}
