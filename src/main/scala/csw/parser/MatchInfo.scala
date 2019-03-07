package csw.parser

import csw.parser.Res.Success

case class MatchInfo[T](content: Content, matchStart: ContentIndex, result: Success[T]) {
  def matchEnd: ContentIndex = result.index

  def value: T = result.value
}
