package learnfp.transformer

import learnfp.functor.Functor
import learnfp.functor.Maybe.{Just, Maybe, Nothing}
import learnfp.monad.{Monad, MonadOps}

case class MaybeT[A, M[_]](runMaybeT: M[Maybe[A]])(implicit f: Functor[M], m: Monad[M])

object MaybeT {
  implicit def maybeTFunctorInstance[M[_]](implicit f: Functor[M], m: Monad[M]) = new Functor[({type E[X] = MaybeT[X, M]})#E] {
    override def fmap[A, B](a: MaybeT[A, M])(fx: A => B): MaybeT[B, M] = MaybeT(f.fmap(a.runMaybeT) {
      case Just(value) => Just(fx(value))
      case Nothing() => Nothing()
    })
  }

  implicit def maybeTMonadInstance[M[_]](implicit f: Functor[M], m: Monad[M]) = new Monad[({type E[X] = MaybeT[X, M]})#E]() {
    override def pure[A](a: A): MaybeT[A, M] = MaybeT(m.pure(Just(a)))

    override def flatMap[A, B](a: MaybeT[A, M])(fx: A => MaybeT[B, M]): MaybeT[B, M] = {
      MaybeT(
        m.flatMap(a.runMaybeT)({
          case Nothing() => m.pure(Nothing[B]())
          case Just(av) => fx(av).runMaybeT
        }))
    }
  }

  implicit def maybeTToMonadOps[A, M[_]](a: MaybeT[A, M])(implicit m: Monad[M], f: Functor[M]) =
    new MonadOps[A, ({type E[X] = MaybeT[X, M]})#E](a)

  implicit def maybeTMonadTransInstance[M[_]](implicit f: Functor[M], m: Monad[M]) = new MonadTransformer[M, MaybeT] {
    override def lift[A](a: M[A]): MaybeT[A, M] = MaybeT(f.fmap(a)(x => Just(x)))
  }

  def nothingT[A, M[_]](implicit f: Functor[M], m: Monad[M]): MaybeT[A, M] = MaybeT(m.pure(Nothing()))

  def lift[A, M[_]](a: M[A])(implicit f: Functor[M], m: Monad[M]): MaybeT[A, M] = maybeTMonadTransInstance.lift(a)
}
