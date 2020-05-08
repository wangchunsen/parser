package csw.parser

import csw.parser.Res._

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions

trait ParserPredef {
  implicit def stringParser(str: String): PUnit = sequenceParser(str.toCharArray)

  def sequenceParser(sequence: Array[Char]): PUnit =
    (context: Content, index: ContentIndex) => {
      val (newCur, charCount) = context.`while`((char, index, _) => {
        index < sequence.length && sequence(index) == char
      }, index)
      if (charCount == sequence.length)
        Success(newCur, ())
      else Fail(s"Expect [${new String(sequence)}] at ${context.display(index)}")
    }

  implicit def charParser(expectChar: Char): PUnit = sequenceParser(Array(expectChar))


  val end: PUnit = (ct: Content, index: ContentIndex) => {
    if (ct.isEnd(index)) Success(index, ())
    else Fail(s"expected end at ${ct.display(index)}")
  }

  /**
    * Match all characters before the specific string
    *
    * @param str the target string to search
    * @return The match result if target string found, or fail if not
    */
  def charsUntil(str: String): PUnit = {
    val target = str.toCharArray
    (ct, index) => {
      val targetIndex = ct.search(target, index)
      if (targetIndex >= 0) Success(targetIndex, ())
      else Fail(s"expect $str to be found from ${ct.display(index)}")
    }
  }

  def matchAll: PUnit = (ct, _) => Success(ct.end, ())

  /**
    * Match all the characters while the function not return false
    *
    * @param fun A predicate function, determining whether the current character should be matched
    * @param limit
    * @param pre
    * @return
    */
  def charsWhile(fun: Char => Boolean,
                 limit: Int = -1,
                 pre: Option[Predicate] = None): Parser[Unit] =
    if (limit < 0 && pre.isEmpty) (ct, index) => {
      val (newCur, _) = ct.`while`((c, _, _) => fun(c), index)
      Success(newCur, ())
    } else (ct, index) => {
      val (newCur, _) = ct.`while`((c, count, contentIndex) => {
        (limit < 0 || count < limit) &&
          fun(c) &&
          (pre.isEmpty || !pre.get(ct, contentIndex))
      }, index)
      Success(newCur, ())
    }


  def charsWhileIn(chars: Seq[Char]*): PUnit = charsWhile(c => chars.exists(_.contains(c)))

  def charsWhileIn(chars: String): PUnit = {
    val range: Parser[NumericRange[Char]] =
      (anyChar.cap ~ "-" ~ anyChar.cap.!!) map (t => {
        val (start, end) = t._1.charAt(0) -> t._2.charAt(0)
        start to end
      }) require (
        info =>
          if (info.value.nonEmpty)
            Some(s"Range start bounds should not smaller than end bounds, ${info.value.start} - ${info.value.end}")
          else None
        ) withName "parser range"


    val charsSeqParser: Parser[Seq[Seq[Char]]] =
      (range | anyChar.cap).rep <~ end map { seq =>
        val ranges: Seq[NumericRange[Char]] =
          seq.filter(_.isInstanceOf[NumericRange[_]]).asInstanceOf[Seq[NumericRange[Char]]]
        val listChars: Seq[Char] = seq.filter(_.isInstanceOf[String]).map(_.toString.charAt(0))

        if (listChars.isEmpty) ranges
        else ranges :+ listChars
      }

    parse(chars, charsSeqParser) match {
      case Success(_, s) => charsWhileIn(s: _*)
      case Fail(reason, _, _) =>
        throw new Exception(s"Illegal parameter value $reason")
    }

  }

  def charsWhileNotIn(chars: Seq[Char]*): PUnit = charsWhile(c => !chars.exists(_.contains(c)))

  def charsWhileInWithLimit(chars: Seq[Char]*)(limit: Int): PUnit =
    charsWhile(c => chars.exists(_.contains(c)), limit)

  /**
    * Match any exact one char
    *
    * @return
    */
  def anyChar: PUnit = charsWhile(_ => true, limit = 1).min(1)

  def void(content: => Any): PUnit = (_, index) => {
    content
    Success(index, ())
  }

  def pass: PUnit = (_, index) => Success(index, ())

  def pass[T](value: => T): Parser[T] = (_, index) => Success(index, value)

  def reject[T](reason: => String): Parser[T] = (_, _) => Fail(reason)

  def error(reason: => String): PUnit = (_, _) => Fail("manual error raised", isError = true)
}
