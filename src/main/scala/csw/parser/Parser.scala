package csw.parser

import csw.parser.Res._
import scala.collection.immutable.Seq

object Parser {

  private class OrParser[T](parses: Seq[Parser[T]]) extends Parser[T] {
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

  implicit class ParserOps[T](val self: Parser[T]) extends AnyVal {
    def withName(implicit name: ParseName): Parser[T] = {
      def wrapFail(f: Fail) = Fail(f.index, s"For {${name.name}}, ${f.reason}")

      (ct, index) =>
        self(ct, index) match {
          case f: Fail => wrapFail(f)
          case e@Error(f, _) => e.copy(cause = wrapFail(f))
          case a => a
        }
    }

    def map[V](fun: T => V): Parser[V] = (ct, c) => self(ct, c) map fun

    def require(fun: (Content, ContentIndex, Success[T]) => Boolean, msg: (Content, ContentIndex, Success[T]) => String): Parser[T] =
      mapSuccess[T, T](self, (s, ct, startCur) => {
        if (fun(ct, startCur, s)) s
        else Fail(startCur, msg(ct, startCur, s))
      })

    def flatMap[V](fun: T => Parser[V]): Parser[V] =
      Parser.mapSuccess[T, V](self, (s, ct, _) => fun(s.value)(ct, s.index))

    def ~[V, R](other: => Parser[V])(implicit cb: Combiner[T, V, R]): Parser[R] =
      Parser.mapSuccess[T, R](self, (s, ct, _) =>
        other(ct, s.index) map { v =>
          cb(s.value, v)
        })

    def ~![R](other: => Parser[R]): Parser[R] =
      Parser.mapSuccess[T, R](self, (s, ct, _) => other(ct, s.index))

    def cap: Parser[String] =
      Parser.mapSuccess[T, String](self, (s, ct, originIndex) => Success(s.index, ct.diff(s.index, originIndex).toString))

    def asError: Parser[T] = (ct, cur) => {
      self(ct, cur) match {
        case f: Fail => Error(f, "")
        case o => o
      }
    }

    def min(size: Int): Parser[T] =
      Parser.mapSuccess[T, T](self, (success, _, index) => {
        val getSize = Math.abs(success.index - index)
        if (getSize < size) Fail(index, s"expect min of $size characters, but get $getSize")
        else success
      })

    def max(size: Int): Parser[T] =
      Parser.mapSuccess[T, T](self, (success, _, originIndex) => {
        val getSize = Math.abs(success.index - originIndex)
        if (getSize > size) Fail(originIndex, s"expect max of $size characters, but get $getSize")
        else success
      })

    def opt[V](implicit tp: ToOption[T, V]): Parser[V] = (ct, cur) => {
      self(ct, cur) match {
        case f: Fail => Success(cur, tp.empty)
        case e: Error => e
        case Success(cur1, value) => Success(cur1, tp.ofValue(value))
      }
    }

    def or[P >: T](other: => Parser[P]): Parser[P] = self match {
      case o: OrParser[T] => o :+ other
      case _ => new OrParser(Seq(self, other))
    }

    def rep[R](implicit ev: AppendAble[T, R]): Parser[R] = loopWhileSuccess(self)(ev)

    def rep[R](delimiter: Parser[_])(implicit ev: AppendAble[T, R]): Parser[R] =
      self flatMap { res => loopWhileSuccess(delimiter ~! self)(ev append res) }
  }

  private def mapSuccess[T, R](parse: Parser[T], fun: (Success[T], Content, ContentIndex) => Res[R]): Parser[R] =
    (ct, cur) => parse(ct, cur) match {
      case s: Success[T] => fun(s, ct, cur)
      case o => o.asInstanceOf[Res[R]]
    }

  private def loopWhileSuccess[T, R](parser: Parser[T])(ev: AppendAble[T, R]): Parser[R] = (ct, cur) => {
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

  def run[T](context: Content, p: Parser[T]): Res[T] = p(context, 0)

  def isMatches[T](parser: Parser[T], context: Content, index: ContentIndex): Boolean =
    parser(context, index) match {
      case _: Success[_] => true
      case _ => false
    }

  def cap[T](parser: PUnit): Parser[String] = parser.cap

}
