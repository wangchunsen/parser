package parser
import scala.collection.immutable.Seq
import Res._

trait Parser[+T] {
  self =>
  def parse(context: Context, cur: Cur): Res[T]

  def naming(implicit name:Parser.Name):Parser[T] = {
    def wrapFail(f:Fail) =  Fail(f.cur, s"For {$name}, ${f.reason}")
    (ct, cur)=> self.parse(ct,cur) match{
      case f:Fail => wrapFail(f)
      case e@Error(f, _) => e.copy(cause = wrapFail(f))
      case a => a
    }
  }

  def map[V](fun: T => V): Parser[V] = (ct, c) => self.parse(ct, c) map fun

  def flatMap[V, R](fun: T => Parser[V])(
      implicit cb: Combiner[T, V, R]): Parser[R] =
    Parser.mapSuccess(self, (s, ct, cur) => {
      val next = fun(s.value)
      Parser.onSucess(next.parse(ct, s.cur),
                      s1 => Success(s1.cur, cb(s.value, s1.value)))
    })

  def ~[V, R](other: => Parser[V])(implicit cb: Combiner[T, V, R]): Parser[R] =
    Parser.mapSuccess(self,
                      (s, ct, _) =>
                        other.parse(ct, s.cur) map { v =>
                          cb(s.value, v)
                      })

  def cap: Parser[String] =
    Parser.mapSuccess(
      self,
      (s, ct, cur) => Success(s.cur, ct.diff(s.cur, cur).toString))

  def asError: Parser[T] = (ct, cur) => {
    self.parse(ct, cur) match {
      case f: Fail => Error(f, "")
      case o       => o
    }
  }

  def min(size: Int): Parser[T] =
    Parser.mapSuccess(
      self,
      (success, originCur) => {
        val getSize = success.cur - originCur
        if (getSize < size)
          Fail(
            originCur,
            s"expect min of $size characters, but get $getSize")
        else success
      }
    )

  def max(size: Int): Parser[T] =
    Parser.mapSuccess(
      self,
      (success, originCur) => {
        val getSize = success.cur - originCur
        if (getSize > size)
          Fail(
            originCur,
            s"expect max of $size characters, but get $getSize")
        else success
      }
    )

  def ?[V](implicit tp: ToOption[T, V]): Parser[V] = (ct, cur) => {
    self.parse(ct, cur) match {
      case f: Fail              => Success(cur, tp.empty)
      case e: Error             => e
      case Success(cur1, value) => Success(cur1, tp.ofValue(value))
    }
  }

  def or[P >: T](other: => Parser[P]): Parser[P] =
    new OrParser(Seq(self, other))

  def rep[R](implicit evic: ToSeq[T, R]): Parser[R] = new Parser[R] {
    override def parse(ct: Context, cur: Cur): Res[R] = {
      var c = cur
      var looping = true
      var res: R = evic.empty
      while (looping) self.parse(ct, c) match {
        case Success(newCur, value) =>
          c = newCur
          res = evic.append(res, value)
        case e: Error => return e
        case _        => looping = false
      }
      Success(c, res)
    }
  }
}

class OrParser[T](parses: Seq[Parser[T]]) extends Parser[T] {
  def succesOrError[T](res: Res[T]) = res match {
    case f: Fail => false
    case _       => true
  }
  def parse(context: Context, cur: Cur): Res[T] = {
    val stream = parses.toStream.map(_.parse(context, cur))
    var resStream = stream
    while (resStream.nonEmpty) {
      resStream match {
        case #::(head, tail) =>
          if (succesOrError(head)) return head
          else resStream = tail
        case _ => resStream = Stream.empty
      }
    }
    def failMsg =
      "all fialed: " + stream.toSeq
        .map(res => s"[${res.asInstanceOf[Fail].reason}]")
        .mkString(",")
    Fail(cur, failMsg)
  }

  override def or[P >: T](other: => Parser[P]): Parser[P] =
    new OrParser[P](parses :+ other)
}

object Parser {
  opaque type Name = String
  object Name{
    def apply(str:String):Name = str
    inline implicit def auto:Name = ~macros.parserNameImpl
  }

  def onSucess[T, R](res: Res[T], fun: Success[T] => Res[R]): Res[R] =
    res match {
      case s: Success[T] => fun(s)
      case o             => o.asInstanceOf[Res[R]]
    }

  inline def mapSuccess[T, R](parse: Parser[T],
                              fun: Success[T] => Res[R]): Parser[R] =
    mapSuccess(parse, (s, _, _) => fun(s))

  inline def mapSuccess[T, R](parse: Parser[T],
                              fun: (Success[T], Cur) => Res[R]): Parser[R] =
    mapSuccess(parse, (s, _, cur) => fun(s, cur))

  def mapSuccess[T, R](parse: Parser[T],
                       fun: (Success[T], Context, Cur) => Res[R]): Parser[R] =
    (ct, cur) =>
      parse.parse(ct, cur) match {
        case s: Success[T] => fun(s, ct, cur)
        case o             => o.asInstanceOf[Res[R]]
    }

  def run[T](context: Context, p: Parser[T]): Res[T] =
    p.parse(context, Cur(1, 0, -1))

  def matches[T](parser: Parser[T], context: Context, cur: Cur): Boolean =
    parser.parse(context, cur) match {
      case s: Success[_] => true
      case _             => false
    }
}
