package parser

import Parsers._
import parser.doc.{Comment, Element, Node, Text}
import scala.language.implicitConversions

import scala.collection.immutable.ListMap
import scala.io.Source

object MyParser {
  type AttrValue = (String, Option[String])
  val voidTags = Set("meta", "link", "img", "input", "br", "hr")

  def isWhiteSpace(char: Char) = " \r\n\t".contains(char)

  def nameing: Parser[String] =
    charWhileIn('a' to 'z', 'A' to 'Z', '0' to '9', "_-:").cap

  def attributeName =
    charWhile(char => {
      char > ' ' && char <= '~' && !Array('=', '&', '/', '<', '>').contains(
        char
      )
    }).min(1).cap

  def maybeSpace = charWhileIn(" \r\n\t")

  def mustSpace = charWhileIn(" \r\n\t") min 1

  def attribute: Parser[AttrValue] =
    mustSpace ~ attributeName ~ (maybeSpace ~ "=" ~ maybeSpace ~ attrValue).?

  def attributes: Parser[Seq[AttrValue]] = attribute.rep

  def attrValue =
    (("'".charAt(0): Parser[Unit]) or '"').cap.? flatMap { quote =>
      quote
        .map { q =>
          val char = q.charAt(0)
          charWhile(c => c != char, "attribute value in quote").cap ~ char
        }
        .getOrElse {
          charWhile(
            c => !isWhiteSpace(c),
            "no quoteattribute value",
            pre = Some(">" or "/>")
          ).cap
        }
    } map (_._2) asError

  def text: Parser[Text] =
    charWhile(c => c != '<', naming = "text").cap map Text

  def node: Parser[Node] = comment or element or text

  def closeType: Parser[Boolean] =
    maybeSpace ~ ("/>" ~ pass(true) or ('>' ~ pass(false))).asError

  def comment: Parser[Comment] =
    "<!--" ~ (charsUntilStr("-->", include = true).cap map { str =>
      val content = str.substring(0, str.length - "-->".length)
      Comment(content)
    })

  def scriptElement: Parser[Seq[Node]] =
    charsUntilStr("</script>").cap.map { str =>
      Seq(Text(str))
    }

  def element: Parser[Element] =
    '<' ~ nameing ~ attributes ~ closeType flatMap ((tagName, _, closed) => {
      val close: Parser[Unit] =
        if (voidTags(tagName) || closed) pass else s"</$tagName>".asError
      val children = if (tagName == "script") scriptElement else node.rep
      children ~ close
    }) map { (tagName, attris, _, children) =>
      Element(tagName, ListMap(attris: _*), children)
    }

  def run(): Unit = {
    val content = Source.fromFile("/Users/cswang/Desktop/demo.html").mkString
    // val content ="""<path />"""
    val start = System.currentTimeMillis()
    val toopTime = 1000

    0 to toopTime foreach { _ =>
      Parser.run(content, element)
    }

    // println(Parser.run(content, element))
    // val res = Parser.run(" />", closeType)
    // println(res)
    val averageTime = (System.currentTimeMillis() - start) / toopTime

    println(averageTime)
  }
}
