package csw.parser

trait NU[T]

object NU {
  implicit def anyRef[T <: AnyRef]: NU[T] = null.asInstanceOf[NU[T]]

  implicit def bool: NU[Boolean] = null.asInstanceOf[NU[Boolean]]

  implicit def int: NU[Int] = null.asInstanceOf[NU[Int]]

  implicit def long: NU[Long] = null.asInstanceOf[NU[Long]]

  implicit def double: NU[Double] = null.asInstanceOf[NU[Double]]

  implicit def char: NU[Char] = null.asInstanceOf[NU[Char]]
}


trait Combiner[-T, -V, +R] {
  def apply(t: T, v: V): R
}

trait LowerPriorityCombiner {
  implicit def toTuple[T, V](implicit evt: NU[T], evv: NU[V]): Combiner[T, V, (T, V)] = (t: T, v: V) => t -> v
}

object Combiner extends LowerPriorityCombiner {
  implicit def unitWith[T]: Combiner[Unit, T, T] = (_: Unit, v: T) => v

  implicit def withUnit[T](implicit ev: NU[T]): Combiner[T, Unit, T] = (t: T, _: Unit) => t

  implicit def t2left[T1, T2, R](implicit ev: NU[R]): Combiner[(T1, T2), R, (T1, T2, R)] = (t, v) => (t._1, t._2, v)

  implicit def t2right[T1, T2, R](implicit ev: NU[R]): Combiner[R, (T1, T2), (R, T1, T2)] = (r, t) => (r, t._1, t._2)

  implicit def t3left[T1, T2, T3, R](implicit ev: NU[R]): Combiner[(T1, T2, T3), R, (T1, T2, T3, R)] = (t, v) => (t._1, t._2, t._3, v)

  implicit def t3Right[T1, T2, T3, R](implicit ev: NU[R]): Combiner[R, (T1, T2, T3), (R, T1, T2, T3)] = (r, t) => (r, t._1, t._2, t._3)

  implicit def t4left[T1, T2, T3, T4, R](implicit ev: NU[R]): Combiner[(T1, T2, T3, T4), R, (T1, T2, T3, T4, R)] = (t, v) => (t._1, t._2, t._3, t._4, v)

  implicit def t4Right[T1, T2, T3, T4, R](implicit ev: NU[R]): Combiner[R, (T1, T2, T3, T4), (R, T1, T2, T3, T4)] = (r, t) => (r, t._1, t._2, t._3, t._4)
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

  implicit def ofValue[T](implicit ev: NU[T]): ToOption[T, Option[T]] = notUnit.asInstanceOf[ToOption[T, Option[T]]]
}

trait AppendAble[-T, C] {
  def append(value: T): AppendAble[T, C]

  def res: C
}

object AppendAble {
  private val unit = new AppendAble[Unit, Unit] {
    override def append(value: Unit): this.type = this

    override def res: Unit = Unit
  }

  private class SeqAppendAble[T](seq: Seq[T]) extends AppendAble[T, Seq[T]] {
    override def append(value: T): SeqAppendAble[T] = new SeqAppendAble[T](seq :+ value)

    override def res: Seq[T] = seq
  }

  private val seqEmpty = new SeqAppendAble[Nothing](Seq.empty)

  implicit def ofUnit: AppendAble[Unit, Unit] = unit

  implicit def emptySeq[T](implicit ev: NU[T]): AppendAble[T, Seq[T]] = seqEmpty.asInstanceOf[AppendAble[T, Seq[T]]]
}