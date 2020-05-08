package csw.parser

import Res._

import ParserOps._

trait ParserOps[T] extends Any {
  def parser: Parser[T]

  /**
    * Give this parser a name, that will displayed in the message of fail or error
    *
    * @param name the name of this parser
    * @return a new parser that have a name
    */
  def withName(name: String): Parser[T] = new NamedParser[T](name, parser)

  /**
    * Map the return value type from type `T` to type `V`
    *
    * @param fun the value mapper
    * @tparam V the type of map result
    * @return a new parser with the result value of type `V`
    */
  def map[V](fun: T => V): Parser[V] = Parser.map(parser)(fun)

  def require(fun: MatchInfo[T] => Option[String], asError: Boolean = true): Parser[T] =
    Parser.mapD(parser) { info =>
      fun(info) match {
        case Some(msg) => Fail(msg, isError = asError)
        case _ => info.result
      }
    }

  def flatMap[V](fun: T => Parser[V]): Parser[V] = Parser.flatMap(parser)(fun)

  def ~[V, R](other: => Parser[V])(implicit cb: Combiner[T, V, R]): Parser[R] =
    Parser.flatMap(parser)(t => Parser.map(other)(v => cb(t, v)))

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
  def ~:[V, R](other: => Parser[V])(implicit cb: Combiner[V, T, R]): Parser[R] =
    Parser.mapD(parser) { info =>
      other(info.content, info.startIndex) match {
        case Success(idx, value) if idx == info.endIndex => Success(idx, cb(value, info.value))
        case Success(idx, _) => Fail(s"expect parser matched success to ${info.content.display(info.endIndex)}")
        case f => f.asInstanceOf[Res[R]]
      }
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
    Parser.mapD(parser) { info =>
      Success(info.endIndex, info.content.diff(info.startIndex, info.endIndex).toString)
    }

  /**
    * Means this is a import parser, any fail of this parser will rise to an error
    * and will lead to fail of the total parser.
    *
    * @return
    */
  def !! : Parser[T] = (ct, cur) => {
    parser(ct, cur) match {
      case f: Fail => f.copy(isError = true)
      case o => o
    }
  }

  def withSize(min: Int = -1, max: Int = Int.MaxValue): Parser[T] =
    require({ info =>
      val getSize = Math.abs(info.endIndex - info.startIndex)
      if (getSize < min) Some(s"expect min of $min characters, but get $getSize at ${info.startDisplay}" )
      else if (getSize > max) Some(s"expect max of $max characters, but get $getSize at ${info.startDisplay}")
      else None
    }, asError = false)

  def min(size: Int): Parser[T] = withSize(min = size)

  def max(size: Int): Parser[T] = withSize(max = size)

  def opt[V](implicit tp: ToOption[T, V]): Parser[V] = (content, index) =>
    this.parser(content, index) match {
      case Success(cur1, value) => Success(cur1, tp.ofValue(value))
      case f: Fail if !f.isError => Success(index, tp.empty)
      case f: Fail => f
    }

  def |[P >: T](other: => Parser[P]): Parser[P] = parser match {
    case o: OrParser[T] => o :+ other
    case _ => new OrParser(Seq(parser, other))
  }

  def rep[R](implicit ev: AppendAble[T, R]): Parser[R] = loopWhileSuccess(parser)(ev)

  def rep[R](delimiter: Parser[_])(implicit ev: AppendAble[T, R]): Parser[R] =
    Parser.flatMap[T, R](parser)(
      res => loopWhileSuccess[T, R](ParserOps(delimiter) ~> parser)(ev append res))
}

private object ParserOps {

  def ofLeft[T, R]: Combiner[T, R, T] = (t, _) => t

  def ofRight[T, R]: Combiner[T, R, R] = (_, r) => r

  def apply[T](parser_ : Parser[T]): ParserOps[T] = new ParserOps[T] {
    override val parser: Parser[T] = parser_
  }

  def loopWhileSuccess[T, R](parser: Parser[T])(ev: AppendAble[T, R]): Parser[R] = (ct, cur) => {
    var c = cur
    var looping = true
    var res = ev
    var error: Fail = null
    while (looping) parser(ct, c) match {
      case Success(newCur, value) =>
        c = newCur
        res = res.append(value)
      case fail: Fail if fail.isError =>
        error = fail
        looping = false
      case _ =>
        looping = false
    }
    if (error != null) error
    else Success(c, res.res)
  }


  class OrParser[T](parses: Seq[Parser[T]]) extends Parser[T] {
    private def successOrError(res: Res[T]) = res match {
      case f: Fail if !f.isError => false
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

      Fail(failMsg)
    }

    def :+[P >: T](other: => Parser[P]): Parser[P] =
      new OrParser[P](parses :+ other)
  }

}