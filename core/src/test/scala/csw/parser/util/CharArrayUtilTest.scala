package csw.parser.util

import org.scalatest.prop.Checkers
import org.scalatest.{FreeSpec, Matchers}

class CharArrayUtilTest extends FreeSpec with Checkers with Matchers{
  "For search" in {
    check((str: String) => {
      CharArrayUtil.search(str.toCharArray, 0, str.toCharArray) == 0
    })
  }

  "For search manual" in {
    val str = "픑䢈"
    CharArrayUtil.search(str.toCharArray, 0, str.toCharArray) shouldBe 0
  }
}
