package csw.parser

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.Checkers

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class ParserSpecification extends FreeSpec with Checkers with Matchers{
  "For string" in {
    check { str: String =>
      val content = Content(str)

      parse(content, cap(str)).toOption.get == str
    }
  }

  "For Or" - {
    "Should execute each parser at most once" in {
      val buff = ArrayBuffer.empty[Int]

      def make(index:Int) =  reject{
        buff.append(index)
        ""
      }
      var p:PUnit = null
      0 to 10 foreach(i =>{
        if (p == null) p = make(i)
        else p = p | make(i)
      })

      parse("", p)
      buff diff (0 to 10) shouldBe empty
    }

    "Should stop parser at the first success one" in {
      val sucessIndex = new Random().nextInt(10)

      def make(index:Int):Parser[Int] =
        if(index == sucessIndex ) pass(index)
        else reject("")

      var p:Parser[Int] = null
      0 to 10 foreach(i =>{
        if (p == null) p = make(i)
        else p = p | make(i)
      })

      val result = parse("", p)
      result.toOption.get shouldBe sucessIndex
    }
  }
}
