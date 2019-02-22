package parser.doc

import scala.collection.immutable.ListMap

case class Document(nodes: Seq[Node])

class Node

case class Text(text: String) extends Node

case class Comment(content: String) extends Node

case class Element(
    tagName: String,
    attributes: ListMap[String, Option[String]] = ListMap.empty,
    children: Seq[Node] = Seq.empty
) extends Node
