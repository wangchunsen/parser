package csw.parser

import csw.parser.Res.{Fail, Success}

object Parser {
  def map[T, V](p: Parser[T])(fun: T => V): Parser[V] = (content, index) => {
    p(content, index) match {
      case Success(index, value) => Success(index, fun(value))
      case f: Fail => f
    }
  }


  def mapD[T, V](p: Parser[T])(fun: MatchInfo[T] => Res[V]): Parser[V] = (content, startIndex) => {
    p(content, startIndex) match {
      case s@Success(_, _) => fun(MatchInfo(content, startIndex, s))
      case f: Fail => f
    }
  }

  def flatMap[T, V](p: Parser[T])(fun: T => Parser[V]): Parser[V] = (content, index) => {
    p(content, index) match {
      case Success(si, value) => fun(value)(content, si)
      case fail: Fail => fail
    }
  }
}
