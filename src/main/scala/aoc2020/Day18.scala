package aoc2020

import scala.collection.mutable.ListBuffer

object Day18 {
  object TokenType extends Enumeration {
    val NUMBER, OPERATOR, PAREN_OPEN, PAREN_CLOSE = Value
  }

  case class Token(val ttype: TokenType.Value, value: Char)

  def tokenize(s: String): List[Token] = {
    var out = ListBuffer.empty[Token]
    for (i <- s.indices) {
      s(i) match {
        case '+' | '*' => out += Token(TokenType.OPERATOR, s(i))
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => out += Token(TokenType.NUMBER, s(i))
        case '(' => out += Token(TokenType.PAREN_OPEN, s(i))
        case ')' => out += Token(TokenType.PAREN_CLOSE, s(i))
        case ' ' =>
      }
    }
    out.toList
  }

  // Shunting-yard algorithm https://en.wikipedia.org/wiki/Shunting-yard_algorithm
  def infixToPostfix(toks: List[Token], opPrecedence: Map[Char, Int]): List[Token] = {
    var rtoks = toks
    var outQueue = ListBuffer.empty[Token]
    var stack = List.empty[Token]
    while (rtoks.nonEmpty) {
      val ct = rtoks.head
      if (ct.ttype == TokenType.NUMBER) outQueue += ct
      else if (ct.ttype == TokenType.OPERATOR) {
        while (
          stack.nonEmpty
            && stack.head.ttype != TokenType.PAREN_OPEN
            && opPrecedence(stack.head.value) >= opPrecedence(ct.value)
        ) {
          outQueue += stack.head
          stack = stack.drop(1)
        }
        stack = ct :: stack
      } else if (ct.ttype == TokenType.PAREN_OPEN) stack = ct :: stack
      else if (ct.ttype == TokenType.PAREN_CLOSE) {
        while (stack.head.ttype != TokenType.PAREN_OPEN) {
          outQueue += stack.head
          stack = stack.drop(1)
        }
        if (stack.head.ttype == TokenType.PAREN_OPEN) stack = stack.drop(1)
      }
      rtoks = rtoks.drop(1)
    }
    while (stack.nonEmpty) {
      outQueue += stack.head
      stack = stack.drop(1)
    }
    outQueue.toList
  }

  def solvePostfix(postToks: List[Token]): Long = {
    var stack = ListBuffer.empty[Long]
    var ptoks = postToks
    while (ptoks.nonEmpty) {
      val ct = ptoks.head
      if (ct.ttype == TokenType.NUMBER) {
        Integer.parseInt("" + ct.value) +=: stack
      } else if (ct.value == '+') {
        stack = ((stack(0) + stack(1)) +: stack.drop(2))
      } else if (ct.value == '*') {
        stack = ((stack(0) * stack(1)) +: stack.drop(2))
      }
      ptoks = ptoks.drop(1)
    }
    stack.head
  }

  def evalLine(s: String, precedences: Map[Char, Int]): Long = {
    solvePostfix(infixToPostfix(tokenize(s), precedences))
  }

  def part1(instrs: List[String]): Long = {
    val precedences = Map('*' -> 0, '+' -> 0)
    instrs.map(evalLine(_, precedences)).sum
  }

  def part2(instrs: List[String]): Long = {
    val precedences = Map('*' -> 0, '+' -> 1)
    instrs.map(evalLine(_, precedences)).sum
  }
}
