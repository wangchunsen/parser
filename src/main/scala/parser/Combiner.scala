package parser

trait NU[T]

object NU {
  erased implicit def anyRef[T <: AnyRef] : NU[T] = null.asInstanceOf[NU[T]]

  erased implicit def bool:NU[Boolean] = null.asInstanceOf[NU[Boolean]]
}


trait Combiner[-T, -V, +R] {
  def apply(t: T, v: V): R
}

trait LowerPriorityCombiner {
  implicit def toTuple[T, V](erased implicit evt:NU[T], evv:NU[V]): Combiner[T, V, (T, V)] = (t: T, v: V) => t -> v
}

object Combiner extends LowerPriorityCombiner {
  implicit def unitWith[T]: Combiner[Unit, T, T] = (_: Unit, v: T) => v

  implicit def withUnit[T](erased implicit ev:NU[T]): Combiner[T, Unit, T] = (t: T, _: Unit) => t

  implicit def t2left[T1 , T2, R](erased implicit ev:NU[R]): Combiner[(T1, T2), R, (T1, T2, R)] = (t, v) => (t._1, t._2, v)

  implicit def t2right[T1, T2 , R](erased implicit ev:NU[R]): Combiner[R, (T1, T2), (R, T1, T2)] = (r, t) => (r, t._1, t._2)

  implicit def t3left[T1, T2, T3, R](erased implicit ev:NU[R]): Combiner[(T1, T2, T3), R, (T1, T2, T3, R)] = (t, v) => (t._1, t._2, t._3, v)

  implicit def t3Right[T1, T2, T3, R](erased implicit ev:NU[R]): Combiner[R, (T1, T2, T3), (R, T1, T2, T3)] = (r, t) => (r, t._1, t._2, t._3)

  implicit def t4left[T1, T2, T3, T4, R](erased implicit ev:NU[R]): Combiner[(T1, T2, T3, T4), R, (T1, T2, T3, T4, R)] = (t, v) => (t._1, t._2, t._3, t._4, v)

  implicit def t4Right[T1, T2, T3, T4, R](erased implicit ev:NU[R]): Combiner[R, (T1, T2, T3, T4), (R, T1, T2, T3, T4)] = (r, t) => (r, t._1, t._2, t._3, t._4)
}

trait ToOption[-T, +R] {
  def ofValue(value: T): R

  def empty: R
}

object ToOption {
  private val toUnit = new ToOption[Unit, Unit] {
    override def ofValue(value: Unit): Unit = ()

    override def empty: Unit = ()
  }
  private val notUnit = new ToOption[Any, Option[Any]] {
    override def ofValue(value: Any): Option[Any] = Some(value)

    override def empty: Option[Any] = None
  }

  implicit def ofUnit: ToOption[Unit, Unit] = toUnit

  implicit def ofValue[T](erased implicit ev: NU[T]): ToOption[T, Option[T]] = notUnit.asInstanceOf[ToOption[T, Option[T]]]
}

trait ToSeq[-T, C] {
  def empty: C

  def append(col: C, value: T): C
}

object ToSeq {
  private val toUnit = new ToSeq[Unit, Unit] {
    override def empty: Unit = ()

    override def append(col: Unit, value: Unit): Unit = ()
  }
  private val notUnit = new ToSeq[Any, Seq[Any]] {
    override def append(col: Seq[Any], value: Any): Seq[Any] = col :+ value

    override def empty: Seq[Any] = Seq.empty
  }

  implicit def ofUnit: ToSeq[Unit, Unit] = toUnit

  implicit def ofNotUnit[T](erased implicit ev:NU[T]): ToSeq[T, Seq[T]] = notUnit.asInstanceOf[ToSeq[T, Seq[T]]]
}