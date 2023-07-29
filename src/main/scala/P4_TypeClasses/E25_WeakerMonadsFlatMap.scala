package P4_TypeClasses

import cats.{Applicative, Apply, FlatMap, Functor}
import cats.syntax.functor._

object E25_WeakerMonadsFlatMap {
  private trait MyFlatMap[T[_]] extends Apply[T] {
    def flatMap[A, B](ta: T[A])(f: A => T[B]): T[B]

    def ap[A, B](wf: T[A => B])(wa: T[A]): T[B] = 
      flatMap(wa)(a => map(wf)(f => f(a)))
  }

  trait MyMonad[T[_]] extends Applicative[T] with FlatMap[T] {
    override def map[A, B](ta: T[A])(f: A => B): T[B] = flatMap(ta)(x => pure(f(x)))
  }
}
