package aoc2019

object IntCode {
  def parseCode(str: String): Array[Int] = {
    str.split(",").map(s => Integer.parseInt(s))
  }

  def opAdd(code: Array[Int], pc: Int): (Array[Int], Int) = {
    val op1Addr = code(pc + 1)
    val op2Addr = code(pc + 2)
    val outAddr = code(pc + 3)
    code(outAddr) = code(op1Addr) + code(op2Addr)
    (code, pc+4)
  }

  def opMultiply(code: Array[Int], pc: Int): (Array[Int], Int) = {
    val op1Addr = code(pc + 1)
    val op2Addr = code(pc + 2)
    val outAddr = code(pc + 3)
    code(outAddr) = code(op1Addr) * code(op2Addr)
    (code, pc+4)
  }

  def runProgram(code: Array[Int]): Array[Int] = {
    var pc = 0
    var opc = code(pc)
    var ncode = code.clone
    while (opc != 99) {
      val(ocode, npc) = opc match {
        case 1 => opAdd(ncode, pc)
        case 2 => opMultiply(ncode, pc)
        case _ => throw new UnsupportedOperationException
      }
      ncode = ocode
      pc = npc
      opc = ncode(pc)
    }
    ncode
  }
}
