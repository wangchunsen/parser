package csw.parser


import csw.parser.util.CharArrayUtil

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object Content {
  implicit def fromString(str: String): Content = apply(str)

  def apply(str: String): Content = {
    val chars = str.toCharArray
    val lineInfoBuffer = ArrayBuffer.empty[Int]
    lineInfoBuffer.append(0)
    for (i <- 0 until chars.length) {
      if (chars(i) == '\n') {
        lineInfoBuffer.append(i + 1)
      }
    }

    new Content(chars, lineInfoBuffer.toArray, chars.length)
  }
}

class Content private(private val chars: Array[Char],
                      private val lineInfo: Array[Int],
                      val end: Int) {
  def endAt(index: ContentIndex): Content =
    new Content(chars = chars, lineInfo = lineInfo, end = index)

  type ContentIndex = Int
  type Count = Int

  private def toLineAndCol(fromLine: Int = 0, index: Int): Option[(Int, Int)] = {
    fromLine.until(lineInfo.length) find { i => lineInfo(i) > index } map { nextLine =>
      val line = nextLine - 1
      val col = index - lineInfo(line)
      line -> col
    } orElse {
      // last line do not have next line
      val lastLine = lineInfo.length - 1

      if (index <= chars.length) Some(lastLine, index - lineInfo(lastLine))
      else None
    }
  }

  def display(index: ContentIndex): String = {
    toLineAndCol(index = index) map { t =>
      val (line, col) = t
      s"(${line + 1}, ${col + 1})"
    } getOrElse "unknown"
  }

  def isEnd(index: ContentIndex): Boolean = index >= end


  /**
    * Search the target char sequence from the content, return the first matched position.
    *
    * @param target the target to search
    * @param index  the index to search start from
    * @return the first matched index, or -1 if not found
    */
  def search(target: Array[Char], index: ContentIndex): ContentIndex =
    CharArrayUtil.search(chars, index, end, target)

  /**
    * Loop until function {@link fun} first return false
    *
    * @param fun   A function that takes tree parameters, and return a bool value indicate whether should the loop stop
    *              <br>Char: The character at current position
    *              <br>Int: The index of this loop
    *              <br>ContentIndex: the index at the whole content
    * @param index the start index of this loop
    * @return a tuple of the contentIndex that the loop stopped and matched character count
    */
  def `while`(fun: (Char, Int, ContentIndex) => Boolean, index: ContentIndex): (ContentIndex, Count) = {
    val offset = index
    var contentIndex = index

    while (contentIndex < end) {
      val char = chars(contentIndex)

      if (!fun(char, contentIndex - offset, contentIndex)) {
        return contentIndex -> (contentIndex - offset)
      }

      contentIndex += 1
    }
    contentIndex -> (contentIndex - offset)
  }

  /**
    * Compare two cursor and return the String between these two cursors
    * <br>Note:the order of these two cursors are not sensitive
    *
    * @param c1 the first cursor, include
    * @param c2 the other cursor, exclude
    * @return String between the two cursors, start cursor include and end cursor exclude
    */
  def diff(index1: ContentIndex, index2: ContentIndex): String =
    new String(chars, Math.min(index1, index2), Math.abs(index1 - index2))
}