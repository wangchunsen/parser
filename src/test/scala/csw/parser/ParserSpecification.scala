package csw.parser

import csw.parser.Parser._
import csw.parser.predef._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.FreeSpec
import org.scalatest.prop.Checkers

class ParserSpecification extends FreeSpec with Checkers {
  "For string" in {
    check { str: String =>
      val content = Content(str)

      Parser.run(content, cap(str)).toOption.get == str
    }
  }
}
