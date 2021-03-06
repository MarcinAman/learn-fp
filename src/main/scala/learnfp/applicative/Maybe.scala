package learnfp.applicative

import learnfp.functor.Maybe.{Just, Maybe, Nothing}
import learnfp.functor.{MaybeInstance => MaybeFunctorInstance}


object MaybeInstance {
  import MaybeFunctorInstance._
  implicit val maybeApplicativeInstance = new Applicative[Maybe] {
    override def pure[A](a: A): Maybe[A] = Just(a)
    override def <*>[A, R](fx: Maybe[A => R])(a: Maybe[A]): Maybe[R] = {
      (a, fx) match {
        case (Just(v), Just(f)) => Just(f(v))
        case _ => Nothing()
      }
    }
  }
}
