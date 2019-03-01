package csw.parser

import org.scalatest.FreeSpec

class ContentTest extends FreeSpec {
  "For diff" -{
    "should return who string between head and end" in {
      def testWhole(str:String):Boolean = {
        val context = Content(str)
        val diffStr = context.diff(0, str.size)
        diffStr == str
      }

      assert(testWhole("123456"))
      assert(testWhole(
        """
          |12
          |234
          |sdf
          |a
        """.stripMargin))
    }

    "should not include end cursor" in {
      val context = Content("1234567")
      assert(context.diff(0, 3) == "123")
    }
  }

  "For display" in {
    val content =
      Content(
      """
        |12
        |3
        |45
        |6""".stripMargin)

    assert(content.display(0) == "(1, 1)")
    assert(content.display(1) == "(2, 1)")
    assert(content.display(3) == "(2, 3)")
    assert(content.display(10) == "(5, 2)")
    assert(content.display(11) == "unknown")
  }
}
