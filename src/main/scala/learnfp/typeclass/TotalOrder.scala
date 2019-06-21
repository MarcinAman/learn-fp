package learnfp.typeclass

trait TotalOrder[A] {
  def less(lhs: A, rhs: A): Boolean
}

object TotalOrderInstances {
  implicit val intInstance: TotalOrder[Int] = new TotalOrder[Int] {
    override def less(lhs: Int, rhs: Int): Boolean = lhs < rhs
  }

  implicit val stringInstance: TotalOrder[String] = new TotalOrder[String] {
    override def less(lhs: String, rhs: String): Boolean = Integer.parseInt(lhs) < Integer.parseInt(rhs)
  }

  implicit def listInstance[T](implicit suborder: TotalOrder[T]): TotalOrder[List[T]] = new TotalOrder[List[T]] {
    override def less(lhs: List[T], rhs: List[T]): Boolean = lhs.zip(rhs).forall(s => suborder.less(s._1, s._2))
  }
}

object Comparator {
  @annotation.implicitNotFound("No instance of TotalOrder found")
  def less[A](lhs: A, rhs: A)(implicit order: TotalOrder[A]) = {
    order.less(lhs, rhs)
  }
}