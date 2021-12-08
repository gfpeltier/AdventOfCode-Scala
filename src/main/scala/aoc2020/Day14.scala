package aoc2020


object Day14 {
  case class MaskPair(setMask: Long, clearMask: Long) {
    def applyMask(value: Long): Long = {
      (value & clearMask) | setMask
    }

    override def toString: String = {
      s"MaskPair{setMask: ${setMask.toHexString}, clearMask ${clearMask.toHexString}}"
    }
  }

  def parseMask(mstr: String): MaskPair = {
    var sm: Long = 0
    var cm: Long = -1
    for (i <- mstr.indices) {
      val mi = mstr.length - (i + 1)
      mstr.reverse(i) match {
        case '0' => cm &= ~(1L << i)
        case '1' => sm |= (1L << i)
        case 'X' =>
      }
    }
    MaskPair(sm, cm)
  }

  case class AddrMask(setMask: Long, floatBits: Set[Int]) {

    def floatMask: Long = {
      var fm: Long = -1
      for (i <- floatBits) fm &= ~(1L << i)
      fm
    }

    def applyMask(addr: Long): List[Long] = {
      var addrs = List.empty[Long]
      val fixedAddr = (addr & floatMask) | setMask
      for (bitSet <- floatBits.subsets()) {
        addrs = addrs :+ bitSet.foldLeft(fixedAddr)((a, bo) => a | (1L << bo))
      }
      addrs
    }

    override def toString: String = {
      s"AddrMask{setMask: ${setMask.toHexString}, floatBits: $floatBits}"
    }
  }

  def parseAddrMask(mstr: String): AddrMask = {
    var sm: Long = 0
    var fb = Set.empty[Int]
    for (i <- mstr.indices) {
      mstr.reverse(i) match {
        case '1' => sm |= (1L << i)
        case 'X' => fb += i
        case '0' =>
      }
    }
    AddrMask(sm, fb)
  }

  def part1(instrs: List[String]): Long = {
    var mem = Map.empty[Int, Long]
    val maskRegex = raw"mask = ([01X]+)".r
    val memRegex = raw"mem\[(\d+)\] = (\d+)".r
    var cMask = MaskPair(0, -1)
    for (s <- instrs) {
      s match {
        case maskRegex(mstr) => {
          cMask = parseMask(mstr)
          println(s"nmask => $mstr :: $cMask")
        }
        case memRegex(addr, value) => {
          println(s"mem => addr: $addr, val: $value")
          mem += (Integer.parseInt(addr) -> cMask.applyMask(java.lang.Long.parseLong(value)))
        }
      }
    }
    mem.values.sum
  }

  def part2(instrs: List[String]): Long = {
    var mem = Map.empty[Long, Long]
    val maskRegex = raw"mask = ([01X]+)".r
    val memRegex = raw"mem\[(\d+)\] = (\d+)".r
    var cMask = AddrMask(0, Set.empty[Int])
    for (s <- instrs) {
      s match {
        case maskRegex(mstr) => cMask = parseAddrMask(mstr)
        case memRegex(addr, value) => {
          val addrs = cMask.applyMask(addr.toLong)
          val lval = value.toLong
          mem = cMask.applyMask(java.lang.Long.parseLong(addr)).foldLeft(mem)((m, a) => m + (a -> lval))
        }
      }
    }
    mem.values.sum
  }
}
