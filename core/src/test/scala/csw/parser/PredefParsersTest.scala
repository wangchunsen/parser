package csw.parser

import csw.parser.Res.Success
import org.scalatest.{FreeSpec, Matchers}

class PredefParsersTest extends FreeSpec with Matchers {
  "For string parser" - {

    "should match empty string to empty string" in {
      val context = Content("")
      assert(isMatches("", context, 0))
    }

    "should match given string and return cursor after string" in {
      val parser: PUnit = "12345"
      val context = Content("12345")
      val res = parse("12345", parser)

      assert(res.isInstanceOf[Success[_]])
      assert(context.isEnd(res.asInstanceOf[Success[_]].index))
    }
  }

  "For charUntil" - {
    "should return match to the cursor target str first appear" in {
      val res = parse("abcabcabcabcaa123sdfsdf", charsUntil("123"))
      assert(res.isInstanceOf[Success[_]])
      assert(res.asInstanceOf[Success[_]].index == 14)
    }
  }

  "For char sequence" - {
    "should match if the current string start current position is equal to target string" in {
      val res = parse("123abcd", "123")
      assert(res.isInstanceOf[Success[_]])
      assert(res.asInstanceOf[Success[_]].index == 3)
    }

    "should match to context end" in {
      val context = Content("123abcd")
      val res = parse(context, "123abcd")
      assert(res.isInstanceOf[Success[_]])
      assert(context.isEnd(res.asInstanceOf[Success[_]].index))
    }

    "should match multiple lines" in {
      val context = Content(
        """
          |123
          |abc
          |@@@
        """.stripMargin)
      val res = parse(context, "\n123\nabc\n")
      assert(res.isInstanceOf[Success[_]])
      assert(res.asInstanceOf[Success[_]].index == 9)
    }

    "should match to end of multiple lines" in {
      val context = Content(
        """
          |123
          |abc
          |@@@
          |""".stripMargin)
      val res = parse(context, "\n123\nabc\n@@@\n")
      assert(res.isInstanceOf[Success[_]])
      assert(context isEnd res.asInstanceOf[Success[_]].index)
    }
  }

  "For char while" in {
    val context = Content("123")
    val value: Res[String] = parse(context, charsWhileIn('0' until '9').cap)
    assert(value.toOption.get == "123")
  }

  "For rep" - {
    "should match as many as possible" in {
      val context = Content("123123")
      val value: Res[Seq[String]] = parse(context, cap("123").rep)

      assert(value.isInstanceOf[Success[_]])
      assert(value.asInstanceOf[Success[Seq[String]]].value.size == 2)
    }

    "should support delimiter" in {
      val context = Content("123,456,789")
      val value: Res[Seq[String]] = parse(context, charsWhileIn('0' to '9').cap.rep(","))

      assert(value.isInstanceOf[Success[_]])
      assert(value.toOption.get == Seq("123", "456", "789"))
    }
  }

  "For charsWhileIn" - {
    "should support ranges" in {
      val p = charsWhileIn("a-z").cap

      val str = new String('a' to 'z' toArray)
      val result = parse(str, p)

      result.toOption.get shouldBe str
    }
    "should support chars list" in {
      val p = charsWhileIn("abcd").cap

      val str = "abcdbcadddac"
      val result = parse(str, p)

      result.toOption.get shouldBe str
    }

    "should support mix" in {
      val p = charsWhileIn("a-z_0-9,").cap ~ end

      val matched = "abcded123,_09"
      val unMatched = "abcded123,_09A"
      parse(matched, p).toOption.get shouldBe matched
      parse(unMatched, p).toOption shouldBe empty
    }

    "should throw error when illegal" in {
      an[Exception] should be thrownBy charsWhileIn("b-a").cap
    }
  }

  "For ~:" - {
    val contenStr = "abcdefg)"
    val p = charsWhileIn("a-z").cap ~: charsUntil(")")

    val result = parse(contenStr, p)

    result.toOption.get shouldBe "abcdefg"
  }
}
