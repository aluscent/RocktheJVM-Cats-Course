package P4_TypeClasses

import cats.{Applicative, Functor, Semigroupal}

object E24_Apply {

  trait MyApply[F[_]] extends Functor[F] with Semigroupal[F] {
    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      ap(map(fa)(a => (b: B) => (a, b)))(fb)

    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

    def mapN[A, B, C](tuple: (F[A], F[B]))(f: (A, B) => C): F[C] =
      map(product(tuple._1, tuple._2))(ab => f(ab._1, ab._2))
  }

  trait MyApplicative[F[_]] extends MyApply[F] {
    def pure[A](x: A): F[A]
  }


  import cats.Apply
  import cats.syntax.apply._
  val listApply = Apply[List]
  val tupleList = (List(24,3), List(65), List(9,43,76)).tupled


  def main(args: Array[String]): Unit = {

  }
}
