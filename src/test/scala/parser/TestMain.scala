package parser

import Parsers._

object TestMain {
  def paserIntAA = (charWhileIn('0' to '9') min 5).naming
  def main(args:Array[String]):Unit ={
    println(Parser.run("1234asdf", paserIntAA))
  }
}
