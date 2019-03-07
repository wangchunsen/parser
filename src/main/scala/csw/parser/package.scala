package csw

import csw.parser.Res.Success

import scala.language.implicitConversions

package object parser extends ParserPredef with PredicatePredef {
  type ContentIndex = Int
  type Parser[+T] = (Content, ContentIndex) => Res[T]
  type PUnit = Parser[Unit]
  type Predicate = (Content, ContentIndex) => Boolean

  implicit class Ops[T](val parser: Parser[T]) extends AnyVal with ParserOps[T]

  implicit class POps(val predicate: Predicate) extends AnyVal with PredicateOps

  implicit def toOps(str: String): ParserOps[Unit] = Ops(stringParser(str))

  implicit def toPOps(str: String): PredicateOps = POps(stringPredicate(str))

  def parse[T](context: Content, p: Parser[T]): Res[T] = p(context, 0)

  def isMatches[T](parser: Parser[T], context: Content, index: ContentIndex): Boolean =
    parser(context, index) match {
      case _: Success[_] => true
      case _ => false
    }

  def cap[T](parser: PUnit): Parser[String] = parser.cap

  def p[T](parser: Parser[T])(implicit name: ParseName) = parser.withName(name.name)
}
