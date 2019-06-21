package learnfp.comonad

import learnfp.functor.Id

object IdInstance {
  implicit val idCoMonadInstance = new CoMonad[Id] {
    override def counit[A](a: Id[A]): A = a.value
    override def coFlatMap[A, B](a: Id[A])(fx: Id[A] => B): Id[B] = Id(fx(a))
  }
}


