package csw.parser

import csw.parser.Res._

import scala.language.implicitConversions

trait Predef {

  implicit def matchStringParser(str: String): PUnit = sequenceParser(str.toCharArray)

  def sequenceParser(sequence: Array[Char]): PUnit =
    (context: Content, index: ContentIndex) => {
      val (newCur, charCount) = context.`while`((char, index, _) => {
        index < sequence.length && sequence(index) == char
      }, index)
      if (charCount == sequence.length)
        Success(newCur, ())
      else Fail(index, s"expect $sequence at ${context.display(index)}")
    }

  implicit def charParser(expectChar: Char): PUnit = sequenceParser(Array(expectChar))

  def sequencePredicate(sequence: Array[Char]): Predicate = {
    (context: Content, index: ContentIndex) => {
      val (_, charCount) = context.`while`((char, index, _) => {
        index < sequence.length && sequence(index) == char
      }, index)
      charCount == sequence.length
    }
  }

  implicit def stringPredicate(str: String): Predicate = sequencePredicate(str.toCharArray)

  val End: PUnit = (ct: Content, index: ContentIndex) => {
    if (ct.isEnd(index)) Success(index, ())
    else Fail(index, s"expected end at ${ct.display(index)}")
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
      else Fail(targetIndex, s"expect $str to be found from ${ct.display(index)}")
    }
  }

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
                 pre: Option[Predicate] = None
                ): Parser[Unit] =
    if (limit < 0 && pre.isEmpty) (ct, index) => {
      val (newCur, _) = ct.`while`((c, _, _) => fun(c), index)
      Success(newCur, ())
    } else (ct, index) => {
      val (newCur, _) = ct.`while`((c, index, contentIndex) => {
        (limit < 0 || limit < index) &&
          fun(c) &&
          (pre.isEmpty || !pre.get(ct, contentIndex))
      }, index)
      Success(newCur, ())
    }


  def charsWhileIn(chars: Seq[Char]*): PUnit = charsWhileInWithLimit(chars: _*)(-1)

  def charsWhileInWithLimit(chars: Seq[Char]*)(limit: Int): PUnit =
    charsWhile(c => chars.exists(_.contains(c)), limit)

  def void(content: => Any): PUnit = (_, index) => {
    content
    Success(index, ())
  }

  def pass: PUnit = (_, index) => Success(index, ())

  def pass[T](value: => T): Parser[T] = (_, index) => Success(index, value)

  def error(reason: => String): PUnit = (_, index) => Error(Fail(index, "manual error raised"), reason)
}
