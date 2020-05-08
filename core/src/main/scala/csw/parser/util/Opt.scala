package csw.parser.util

object Opt {
  @inline def when[T](cond: Boolean)(prd: => T): Option[T] = if (cond) Some(prd) else None
}
