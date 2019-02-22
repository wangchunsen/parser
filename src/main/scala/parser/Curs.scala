package parser

import parser.util.Opt

import scala.language.implicitConversions

case class Cur(line: Int, col: Int, index: Int) {
  def -(cur: Cur): Int = index - cur.index

  override def toString = s"($line ,$col)"
}

object Context {
  implicit def fromString(str: String): Context = apply(str)

  def apply(str: String) = new Context(str.toCharArray)
}

class Context private (private val chars: Array[Char]) {
  def next(cur: Cur): Option[Cur] = {
    if (cur.index + 1 < chars.length) {
      val newCur =
        if (charAt(cur) == '\n')
          cur.copy(line = cur.line + 1, col = 0, index = cur.index + 1)
        else cur.copy(col = cur.col + 1, index = cur.index + 1)
      Some(newCur)
    } else None
  }

  def charAt(cur: Cur): Char = if (cur.index < 0) 0 else chars(cur.index)

  def `match`(cur: Cur, chars: CharSequence): Option[Cur] = {
    var cur_ = cur
    var index = 0
    var break = false
    while (index < chars.length && !break) {
      next(cur_) match {
        case Some(newCur) if chars.charAt(index) == charAt(newCur) =>
          index += 1
          cur_ = newCur
        case _ =>
          break = true
      }
    }
    Opt.when(index == chars.length())(cur_)
  }

  def matchWhile(
      cur: Cur,
      fun: Char => Boolean,
      max: Int = -1,
      pre: Option[Parser[_]] = None
  ): Cur = {
    var matched = 0
    var cur_ = cur
    var break = false
    while ((max < 0 || max > matched) && !break) {
      if (pre.isDefined && Parser.matches(pre.get, this, cur_)) {
        break = true
      } else
        next(cur_) match {
          case Some(newCur) if fun(charAt(newCur)) =>
            matched += 1
            cur_ = newCur
          case _ =>
            break = true
        }
    }
    cur_
  }

  def diff(c1: Cur, c2: Cur): CharSequence = {
    if (c1 == c2) ""
    else {
      val (start, end) = if (c1.index < c2.index) c1 -> c2 else c2 -> c1
      new runtime.ArrayCharSequence(chars, start.index + 1, end.index + 1)
    }
  }
}
