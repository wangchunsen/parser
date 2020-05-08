package csw.parser

sealed trait Res[+T]

object Res {

  case class Success[+T](index: ContentIndex, value: T) extends Res[T]

  case class Fail(reason: String, isError: Boolean = false, stack: List[(String, ContentIndex)] = Nil) extends Res[Nothing]

  implicit class ResOps[T](val res: Res[T]) extends AnyVal {
    def toOption: Option[T] = res match {
      case Res.Success(_, value) => Some(value)
      case _ => None
    }
  }

}
