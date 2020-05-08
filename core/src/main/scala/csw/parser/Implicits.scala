package csw.parser

trait NotUnit[T]

object NotUnit {
  implicit def anyRef[T <: AnyRef]: NotUnit[T] = null.asInstanceOf[NotUnit[T]]

  implicit def bool: NotUnit[Boolean] = null.asInstanceOf[NotUnit[Boolean]]

  implicit def int: NotUnit[Int] = null.asInstanceOf[NotUnit[Int]]

  implicit def long: NotUnit[Long] = null.asInstanceOf[NotUnit[Long]]

  implicit def double: NotUnit[Double] = null.asInstanceOf[NotUnit[Double]]

  implicit def char: NotUnit[Char] = null.asInstanceOf[NotUnit[Char]]
}


trait Combiner[-T, -V, +R] {
  def apply(t: T, v: V): R
}

trait LowerPriorityCombiner {
  implicit def toTuple[T, V](implicit evt: NotUnit[T], evv: NotUnit[V]): Combiner[T, V, (T, V)] = (t: T, v: V) => t -> v
}

object Combiner extends LowerPriorityCombiner {
  implicit def unitWith[T]: Combiner[Unit, T, T] = (_: Unit, v: T) => v

  implicit def withUnit[T](implicit ev: NotUnit[T]): Combiner[T, Unit, T] = (t: T, _: Unit) => t

  implicit def t2left[T1, T2, R](implicit ev: NotUnit[R]): Combiner[(T1, T2), R, (T1, T2, R)] = (t, v) => (t._1, t._2, v)

  implicit def t2right[T1, T2, R](implicit ev: NotUnit[R]): Combiner[R, (T1, T2), (R, T1, T2)] = (r, t) => (r, t._1, t._2)

  implicit def t3left[T1, T2, T3, R](implicit ev: NotUnit[R]): Combiner[(T1, T2, T3), R, (T1, T2, T3, R)] = (t, v) => (t._1, t._2, t._3, v)

  implicit def t3Right[T1, T2, T3, R](implicit ev: NotUnit[R]): Combiner[R, (T1, T2, T3), (R, T1, T2, T3)] = (r, t) => (r, t._1, t._2, t._3)

  implicit def t4left[T1, T2, T3, T4, R](implicit ev: NotUnit[R]): Combiner[(T1, T2, T3, T4), R, (T1, T2, T3, T4, R)] = (t, v) => (t._1, t._2, t._3, t._4, v)

  implicit def t4Right[T1, T2, T3, T4, R](implicit ev: NotUnit[R]): Combiner[R, (T1, T2, T3, T4), (R, T1, T2, T3, T4)] = (r, t) => (r, t._1, t._2, t._3, t._4)
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

  implicit def ofValue[T](implicit ev: NotUnit[T]): ToOption[T, Option[T]] = notUnit.asInstanceOf[ToOption[T, Option[T]]]
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

  implicit def emptySeq[T](implicit ev: NotUnit[T]): AppendAble[T, Seq[T]] = seqEmpty.asInstanceOf[AppendAble[T, Seq[T]]]
}