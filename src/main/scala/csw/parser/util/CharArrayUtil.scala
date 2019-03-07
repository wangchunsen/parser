package csw.parser.util

object CharArrayUtil {

  def search(source: Array[Char], offset: Int, target: Array[Char]): Int =
    search(source, offset, source.length, target)

  def search(source: Array[Char], offset: Int, endIndex: Int, target: Array[Char]): Int = {
    if (source.length - offset < target.length) {
      return -1
    }

    var sIndex = offset
    var tIndex = 0
    while (tIndex < target.length && sIndex < endIndex) {
      if (source(sIndex) == target(tIndex)) {
        sIndex += 1
        tIndex += 1
      } else {
        sIndex = sIndex + 1
        tIndex = 0
      }
    }
    if (tIndex == target.length) sIndex - tIndex else -1
  }
}
