package csw.parser

sealed trait Res[+T] {

}

object Res {

  case class Success[+T](index:ContentIndex, value: T) extends Res[T]

  case class Fail(index:ContentIndex, reason: String) extends Res[Nothing]

  case class Error(cause: Fail, msg: String) extends Res[Nothing]

  implicit class ResOps[T](val res: Res[T]) extends AnyVal {
    def isSuccess:Boolean = res.isInstanceOf[Success[_]]

    def toOption: Option[T] = res match {
      case Res.Success(_, value) => Some(value)
      case _ => None
    }

    def map[R](f: T => R): Res[R] = res match {
      case Res.Success(c, v) => Res.Success(c, f(v))
      case r => r.asInstanceOf[Res[Nothing]]
    }
  }

}
