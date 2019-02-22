package parser

sealed trait Res[+T] {
  def toOption: Option[T] = this match {
    case Res.Success(_, value) => Some(value)
    case _                     => None
  }
  def map[R](f: T => R): Res[R] = this match {
    case Res.Success(c, v) => Res.Success(c, f(v))
    case r                 => r.asInstanceOf[Res[Nothing]]
  }
}
object Res {
  case class Success[+T](cur: Cur, value: T) extends Res[T]
  case class Fail(cur: Cur, reason: String) extends Res[Nothing]
  case class Error(cause: Fail, msg: String) extends Res[Nothing]
}
