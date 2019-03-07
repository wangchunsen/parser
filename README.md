# parser
A pure function parser library for general purpose

Here is a example of parsing html using this library:

```scala
type AttrValue = (String, Option[String])
  val voidElements = Array("area", "base", "br", "col", "embed",
    "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr")

  val textElement = Array("script", "style", "textarea", "title")

  val spaceChars = Array(' ', '\t', '\n', '\r', '\f')

  def isWhiteSpace(char: Char) = spaceChars.contains(char)

  def tagName: Parser[String] =
    p(charsWhileIn("a-z0-9A-Z_").cap)

  def attributeName: Parser[String] = p {
    val illegalChars = spaceChars ++ Array('=', '/', '>', '"', '\'')
    charsWhile(char => !illegalChars.contains(char)).min(1).cap
  }

  def maybeSpace: PUnit = charsWhileIn(spaceChars)

  def mustSpace: PUnit = charsWhileIn(spaceChars) min 1

  def attribute: Parser[AttrValue] =
    mustSpace ~ attributeName ~ (maybeSpace ~> "=" ~> maybeSpace ~> attrValue).opt

  def attributes: Parser[Seq[AttrValue]] = attribute.rep

  def attrValue: Parser[String] = p {
    def quotedValue(quote: Char): Parser[String] = p(charsWhile(c => c != quote).cap <~ quote)

    def noQuote: Parser[String] = p {
      val illegalChars = spaceChars ++ Array('\'', '"', '>', '<', '=', '`')
      charsWhile(c => !illegalChars.contains(c)).cap
    }

    ("\"" | "'").cap.opt flatMap { quote =>
      quote
        .map { q => quotedValue(q.charAt(0)) }
        .getOrElse(noQuote)
    }
  }

  def text: Parser[Text] = charsWhile(c => c != '<').cap map Text

  def comment: Parser[Comment] =
    p(allBetween("<!--", "-->") map Comment)

  def node: Parser[Node] = comment | element | text

  def closeType: Parser[Boolean] =
  maybeSpace ~> ("/>" ~> pass(true) | (">" ~> pass(false))).!!

  def allBetween(start: String, end: String): Parser[String] = {
    val content: Parser[String] = matchAll.cap ~: charsUntil(end)
    start ~> content <~ end
  }



  def scriptElement: Parser[Node] =
    p(allBetween("<script>", "</script>") map Text)

  def closeTag: Parser[String] = p("</" ~> tagName.cap <~ maybeSpace <~ ">")

  def element: Parser[Element] =
    "<" ~> tagName ~ attributes ~ closeType flatMap (t => {
      val (tagName, attrs, closed) = t

      def element(children: Seq[Node] = Seq.empty): Element = Element(tagName = tagName, attributes = ListMap(attrs: _*))

      if (closed || voidElements(tagName)) pass(element())
      else {
        val childrenNodes = node.rep
        childrenNodes <~ s"</$tagName>" map {nodes =>
          element(children = nodes)
        }
      }
    })
```