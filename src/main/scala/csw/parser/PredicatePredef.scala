package csw.parser

import scala.language.implicitConversions

trait PredicatePredef {
  implicit def stringPredicate(str: String): Predicate = sequencePredicate(str.toCharArray)

  def sequencePredicate(sequence: Array[Char]): Predicate = {
    (context: Content, index: ContentIndex) => {
      val (_, charCount) = context.`while`((char, index, _) => {
        index < sequence.length && sequence(index) == char
      }, index)
      charCount == sequence.length
    }
  }
}
