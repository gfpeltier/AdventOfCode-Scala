package aoc2020

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Passport(byr: Int,
               iyr: Int,
               eyr: Int,
               hgt: String,
               hcl: String,
               ecl: String,
               pid: String,
               cid: String) {
  def this(byr: Int, iyr: Int, eyr: Int, hgt: String, hcl: String, ecl: String, pid: String) {
    this(byr, iyr, eyr, hgt, hcl, ecl, pid, "")
  }

  def isBirthValid: Boolean = {
    1920 to 2002 contains byr
  }

  def isIssueValid: Boolean = {
    2010 to 2020 contains iyr
  }

  def isExpirationValid: Boolean = {
    2020 to 2030 contains eyr
  }

  def isHeightValid: Boolean = {
    val hregex = raw"(\d+)(in|cm)".r
    hgt match {
      case hregex(numStr, unit) => {
        val num = Integer.parseInt(numStr)
        if (unit == "cm") {
          150 to 193 contains num
        } else {
          59 to 76 contains num
        }
      }
      case _ => false
    }
  }

  def isHairColorValid: Boolean = {
    raw"#[0-9a-f]{6}".r matches hcl
  }

  def isEyeColorValid: Boolean = {
    Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(ecl)
  }

  def isPassportIDValid: Boolean = {
    raw"[0-9]{9}".r matches pid
  }
}

object Passport {

  def fromMap(m: Map[String, String]): Option[Passport] = {
    if (Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid").diff(m.keySet).nonEmpty) {
      None
    } else {
      Some(new Passport(
        Integer.parseInt(m("byr")),
        Integer.parseInt(m("iyr")),
        Integer.parseInt(m("eyr")),
        m("hgt"),
        m("hcl"),
        m("ecl"),
        m("pid")
      ))
    }
  }

  def fromMapValidated(m: Map[String, String]): Option[Passport] = {
    if (Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid").diff(m.keySet).nonEmpty) {
      None
    } else {
      val pass = new Passport(
        Integer.parseInt(m("byr")),
        Integer.parseInt(m("iyr")),
        Integer.parseInt(m("eyr")),
        m("hgt"),
        m("hcl"),
        m("ecl"),
        m("pid")
      )
      if (pass.isBirthValid &&
        pass.isExpirationValid &&
        pass.isIssueValid &&
        pass.isHeightValid &&
        pass.isHairColorValid &&
        pass.isEyeColorValid &&
        pass.isPassportIDValid
      ) {
        Some(pass)
      } else {
        None
      }
    }
  }
}

object Day4 {

  def createPassports(strs: List[String], validateFields: Boolean): List[Passport] = {
    var passes = ListBuffer.empty[Passport]
    var im = mutable.Map.empty[String, String]
    val passParts = raw"([a-z]+):([^\s]+)".r
    val passFn = if (validateFields) Passport.fromMapValidated _ else Passport.fromMap _
    for (l <- strs) {
      if (l.trim.isEmpty) {
        passFn(im.toMap) match {
          case Some(value) => passes += value
          case None =>
        }
        im = mutable.Map.empty[String, String]
      } else {
        val matches = passParts.findAllIn(l)
        while (matches.hasNext) {
          val n = matches.next
          im += (n.split(':')(0) -> n.split(':')(1))
        }
      }
    }
    passFn(im.toMap) match {
      case Some(value) => passes += value
      case None =>
    }
    passes.toList
  }

  def part1(instrs: List[String]): Int = {
    createPassports(instrs, validateFields = false).length
  }

  def part2(instrs: List[String]): Int = {
    createPassports(instrs, validateFields = true).length
  }
}
