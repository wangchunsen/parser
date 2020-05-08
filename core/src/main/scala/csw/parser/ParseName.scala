package csw.parser

import scala.language.experimental.macros

case class ParseName(name: String)

object ParseName {
  implicit def auto: ParseName = macro Macros.parserNameImpl
}