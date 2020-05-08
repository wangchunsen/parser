package csw.lispparser

trait Node

class List extends Node

case class Keyword(name:String)

import csw.htmlparser.P
import csw.htmlparser.P.spaceChars
import csw.parser._

object P {
  val spaceChars = Array(' ', '\t', '\n', '\r', '\f')

  def maybeSpace: PUnit = charsWhileIn(spaceChars)

  def mustSpace: PUnit = charsWhileIn(spaceChars) min 1

  def symbol = charsWhileNotIn(spaceChars, Seq('(', ')', '[', ']')).min(1).cap

  def node:Parser[AnyRef] = list | vector | keyWords | symbol

  def nodes = (node ~ maybeSpace).rep

//  def test = "(" <~ "" ~> ")"

  def list = p("(" ~ maybeSpace ~ nodes ~ maybeSpace ~ ")".!!)

  def vector = "[" ~ maybeSpace ~ nodes ~ maybeSpace ~ "]" map (_.toVector)

  def keyWords = p(":" ~ charsWhileNotIn(spaceChars,"()[]{}#").!!.cap.map(Keyword))
}

object Parser extends App {
  print(parse(
    """
      |(defn max
      |
      |   [[Number] :? :> Number]
      |   (let [maxrec (fn :tailrec [cmax [rests]] )]
      |      (amxrec  values)))
      |""".stripMargin.trim, P.list ~ end))
}