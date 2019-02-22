package parser

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions
import Res._

object Parsers {

  implicit def sequenceParser(sequence: CharSequence): Parser[Unit] =
    (ct, cur) =>
      ct.`match`(cur, sequence) match {
        case Some(newCur) => Success(newCur, ())
        case None =>
          val msg = s"expect $sequence at (${cur.line}, ${cur.col})"
          Fail(cur, msg)
      }

  implicit def charParser(expectChar: Char): Parser[Unit] =
    sequenceParser(Array(expectChar))

  object End extends Parser[Unit] {
    override def parse(ct: Context, cur: Cur): Res[Unit] = {
      ct.matchWhile(cur, _ => true, max = 1) match {
        case None => Success(cur, ())
        case _    => Fail(cur, s"Expected end at $cur")
      }
    }
  }

  def charsUntilStr(str: String, include: Boolean = false): Parser[Unit] = {
    val strArray = str.toCharArray
    (ct, cur) => {
      var lastCur = cur
      var beforeMatch = cur
      var looping = true
      var arrayMatchedIndex = 0
      var failed = false
      while (looping) {
        ct.next(lastCur) match {
          case Some(cCur) =>
            val char = ct.charAt(cCur)
            if (char == strArray(arrayMatchedIndex)) {
              arrayMatchedIndex += 1
            } else {
              arrayMatchedIndex = 0
              beforeMatch = cCur
            }
            lastCur = cCur
          case None =>
            looping = false
            failed = true
        }
        if (arrayMatchedIndex == strArray.length) {
          looping = false
        }
      }

      if (failed) {
        Fail(cur, s"expect str [$str], but not found until end")
      } else Success(if (include) lastCur else beforeMatch, ())
    }
  }

  def charWhile(
      fun: Char => Boolean,
      naming: String = "",
      pre: Option[Parser[_]] = None
  ): Parser[Unit] = (ct, cur) => {
    val newCur = ct.matchWhile(cur, fun, pre = pre)
    Success(newCur, ())
  }

  def charWhileIn(chars: Seq[Char]*): Parser[Unit] =
    charWhile(
      c => chars.exists(_.contains(c)),
      chars
        .map {
          case r: NumericRange[_] => s"${r.start}-${r.end}"
          case o                  => o.toString
        }
        .mkString(",")
    )

  def void(content: => Unit): Parser[Unit] = (_, cur) => {
    content
    Success(cur, ())
  }

  def pass: Parser[Unit] = (_, cur) => Success(cur, ())

  def pass[T](value: => T): Parser[T] = (_, cur) => Success(cur, value)
}
