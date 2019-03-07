package csw.parser

import csw.parser.PredicateOps.OrPredicate

trait PredicateOps extends Any {
  def predicate: Predicate

  def or(other: Predicate): Predicate = predicate match {
    case or: OrPredicate => or :+ predicate
    case o => new OrPredicate(Seq(predicate, o))
  }
}


private object PredicateOps {

  class OrPredicate(predicates: Seq[Predicate]) extends Predicate {
    def :+(predicate: Predicate): OrPredicate = new OrPredicate(predicates :+ predicate)

    override def apply(v1: Content, v2: ContentIndex): Boolean =
      predicates.exists(_ (v1, v2))
  }

}
