package csw.parser

import csw.parser.Res.Success
import org.scalatest.FreeSpec
import csw.parser.predef._
import Parser._

class PredefParsersTest extends FreeSpec {
  "For string parser" - {

    "should match empty string to empty string" in {
      val context = Content("")
      assert(Parser.isMatches("", context, 0))
    }

    "should match given string and return cursor after string" in {
      val parser: PUnit = "12345"
      val context = Content("12345")
      val res = Parser.run("12345", parser)

      assert(res.isSuccess)
      assert(context.isEnd(res.asInstanceOf[Success[_]].index))
    }
  }

  "For charUntil" - {
    "should return match to the cursor target str first appear" in {
      val res = Parser.run("abcabcabcabcaa123sdfsdf", charsUntil("123"))
      assert(res.isSuccess)
      assert(res.asInstanceOf[Success[_]].index == 14)
    }
  }

  "For char sequence" - {
    "should match if the current string start current position is equal to target string" in {
      val res = Parser.run("123abcd", "123")
      assert(res.isSuccess)
      assert(res.asInstanceOf[Success[_]].index == 3)
    }

    "should match to context end" in {
      val context = Content("123abcd")
      val res = Parser.run(context, "123abcd")
      assert(res.isSuccess)
      assert(context.isEnd(res.asInstanceOf[Success[_]].index))
    }

    "should match multiple lines" in {
      val context = Content(
        """
          |123
          |abc
          |@@@
        """.stripMargin)
      val res = Parser.run(context, "\n123\nabc\n")
      assert(res.isSuccess)
      assert(res.asInstanceOf[Success[_]].index == 9)
    }

    "should match to end of multiple lines" in {
      val context = Content(
        """
          |123
          |abc
          |@@@
          |""".stripMargin)
      val res = Parser.run(context, "\n123\nabc\n@@@\n")
      assert(res.isSuccess)
      assert(context isEnd res.asInstanceOf[Success[_]].index)
    }
  }

  "For char while" in {
    val context = Content("123")
    val value: Res[String] = Parser.run(context, charsWhileIn('0' until '9').cap)
    assert(value.toOption.get == "123")
  }

  "For rep" - {
    "should match as many as possible" in {
      val context = Content("123123")
      val value: Res[Seq[String]] = Parser.run(context, cap("123").rep)

      assert(value.isSuccess)
      assert(value.asInstanceOf[Success[Seq[String]]].value.size == 2)
    }

    "should support delimiter" in {
      val context = Content("123,456,789")
      val value: Res[Seq[String]] = Parser.run(context, charsWhileIn('0' to '9').cap.rep(","))

      assert(value.isSuccess)
      assert(value.toOption.get == Seq("123", "456", "789"))
    }
  }
}
