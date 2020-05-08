package csw.parser

import csw.parser.Res.Success

case class MatchInfo[T](content: Content, startIndex: ContentIndex, result: Success[T]) {
  def endIndex: ContentIndex = result.index

  def value: T = result.value

  def startDisplay = content.display(startIndex)
}
