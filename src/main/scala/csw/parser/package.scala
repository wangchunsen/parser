package csw

package object parser{
  type ContentIndex = Int
  type Parser[+T] = (Content, ContentIndex) => Res[T]
  type PUnit = Parser[Unit]
  type Predicate = (Content, ContentIndex) => Boolean

  object predef extends Predef
}
