package csw.parser

import Res._

import ParserOps._

trait ParserOps[T] extends Any {
  def parser: Parser[T]

  /**
    * Give this parser a name, that will displayed in the message of fail or error
    * @param name the name of this parser
    * @return a new parser that have a name
    */
  def withName(name: String): Parser[T] = {
    def wrapFail(f: Fail) = Fail(f.index, s"For {$name}, ${f.reason}")

    (ct, index) =>
      parser(ct, index) match {
        case f: Fail => wrapFail(f)
        case e@Error(f, _) => e.copy(cause = wrapFail(f))
        case a => a
      }
  }

  /**
    * Map the return value type from type `T` to type `V`
    * @param fun the value mapper
    * @tparam V the type of map result
    * @return a new parser with the result value of type `V`
    */
  def map[V](fun: T => V): Parser[V] = (ct, c) => parser(ct, c) map fun

  def require(fun: MatchInfo[T] => Boolean, msg: MatchInfo[T] => String, asError: Boolean = true): Parser[T] =
    mapSuccess[T, T](parser, (s, ct, startCur) => {
      val info = MatchInfo(ct, startCur, s)
      if (fun(info)) s
      else if (asError) Error(Fail(startCur, "require failed"), msg(info))
      else Fail(startCur, msg(info))
    })


  def flatMap[V](fun: T => Parser[V]): Parser[V] = flatMap_(parser, fun)

  def ~[V, R](other: => Parser[V])(implicit cb: Combiner[T, V, R]): Parser[R] =
    mapSuccess[T, R](parser, (s, ct, _) =>
      other(ct, s.index) map { v =>
        cb(s.value, v)
      })

  /**
    * Self first parser
    * Run this parser first and then run the other parser use the sub context
    * than end at the index of this parser stopped.
    * The other parser must match all the text to the end of the sub context
    * This is much like:
    *
    * {{{
    *   val thisParser = ...
    *   val otherParser = ...
    *   thisParser.cap map {str =>parse(str, thatParser).value}
    * }}}
    *
    * But be more efficient and more convenient
    *
    * @param other the other parser that should match success to the index of this parser
    * @tparam R the result type, which the type of combining this and other type
    * @return A new parser with type `R`
    */
  def ~:[V, R](other: Parser[V])(implicit cb: Combiner[V, T, R]): Parser[R] = {
    mapSuccess[T, R](parser, (scs, ct, startIndex) => {
      val ctEndIndex = scs.index
      val context = ct.endAt(ctEndIndex)

      other(context, startIndex) match {
        case Success(idx, value) =>
          if (context.isEnd(idx)) scs.map(v => cb(value, v))
          else Fail(startIndex, s"expect parser matched success to ${ct.display(ctEndIndex)}")
        case f => f.asInstanceOf[Res[R]]
      }
    })
  }

  /**
    * Combine two parser, ignore the left parser and use right one as result
    *
    * @param other the next after this
    * @tparam R the result type of the next parser
    * @return A parser combined left and right
    */
  def ~>[R](other: => Parser[R]): Parser[R] = this.~(other)(ofRight)

  def <~[R](other: => Parser[R]): Parser[T] = this.~(other)(ofLeft)

  def cap: Parser[String] =
    mapSuccess[T, String](parser, (s, ct, originIndex) => Success(s.index, ct.diff(s.index, originIndex).toString))

  /**
    * Means this is a import parser, any fail of this parser will rise to an error
    * and will lead to fail of the total parser.
    *
    * @return
    */
  def !! : Parser[T] = (ct, cur) => {
    parser(ct, cur) match {
      case f: Fail => Error(f, "")
      case o => o
    }
  }

  def min(size: Int): Parser[T] =
    mapSuccess[T, T](parser, (success, _, index) => {
      val getSize = Math.abs(success.index - index)
      if (getSize < size) Fail(index, s"expect min of $size characters, but get $getSize")
      else success
    })

  def max(size: Int): Parser[T] =
    mapSuccess[T, T](parser, (success, _, originIndex) => {
      val getSize = Math.abs(success.index - originIndex)
      if (getSize > size) Fail(originIndex, s"expect max of $size characters, but get $getSize")
      else success
    })

  def opt[V](implicit tp: ToOption[T, V]): Parser[V] = (ct, cur) => {
    parser(ct, cur) match {
      case f: Fail => Success(cur, tp.empty)
      case e: Error => e
      case Success(cur1, value) => Success(cur1, tp.ofValue(value))
    }
  }

  def |[P >: T](other: => Parser[P]): Parser[P] = parser match {
    case o: OrParser[T] => o :+ other
    case _ => new OrParser(Seq(parser, other))
  }

  def rep[R](implicit ev: AppendAble[T, R]): Parser[R] = loopWhileSuccess(parser)(ev)

  def rep[R](delimiter: Parser[_])(implicit ev: AppendAble[T, R]): Parser[R] =
    flatMap_[T, R](
      parser,
      res => loopWhileSuccess[T, R](ParserOps(delimiter) ~> parser)(ev append res))
}

private object ParserOps {

  def ofLeft[T, R]: Combiner[T, R, T] = (t, _) => t

  def ofRight[T, R]: Combiner[T, R, R] = (_, r) => r

  def apply[T](parser_ : Parser[T]): ParserOps[T] = new ParserOps[T] {
    override val parser: Parser[T] = parser_
  }

  def flatMap_[T, V](pt: Parser[T], fun: T => Parser[V]): Parser[V] =
    mapSuccess[T, V](pt, (s, ct, _) => fun(s.value)(ct, s.index))

  def mapSuccess[T, R](parse: Parser[T], fun: (Success[T], Content, ContentIndex) => Res[R]): Parser[R] =
    (ct, cur) => parse(ct, cur) match {
      case s: Success[T] => fun(s, ct, cur)
      case o => o.asInstanceOf[Res[R]]
    }

  def loopWhileSuccess[T, R](parser: Parser[T])(ev: AppendAble[T, R]): Parser[R] = (ct, cur) => {
    var c = cur
    var looping = true
    var res = ev
    var error: Error = null
    while (looping) parser(ct, c) match {
      case Success(newCur, value) =>
        c = newCur
        res = res.append(value)
      case e: Error =>
        error = e
        looping = false
      case _ =>
        looping = false
    }
    if (error != null) error
    else Success(c, res.res)
  }


  class OrParser[T](parses: Seq[Parser[T]]) extends Parser[T] {
    private def successOrError(res: Res[T]) = res match {
      case _: Fail => false
      case _ => true
    }

    override def apply(context: Content, index: ContentIndex): Res[T] = {
      val stream = parses.toStream.map(_ (context, index))
      var resStream = stream
      while (resStream.nonEmpty) {
        resStream match {
          case #::(head, tail) =>
            if (successOrError(head)) return head
            else resStream = tail
          case _ => resStream = Stream.empty
        }
      }

      def failMsg =
        "all failed: " + stream
          .map(res => s"[${res.asInstanceOf[Fail].reason}]")
          .mkString(",")

      Fail(index, failMsg)
    }

    def :+[P >: T](other: => Parser[P]): Parser[P] =
      new OrParser[P](parses :+ other)
  }

}