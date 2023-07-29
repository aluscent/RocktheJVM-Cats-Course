package P4_TypeClasses

object E23_Applicative {

  import cats.Applicative
  val listApplicative = Applicative[List]
  val sampleListNum = listApplicative.pure(4)
  val sampleListChar = listApplicative.pure("a")

  val sampleOption = Applicative[Option].pure(76)

  // pure extension method
  import cats.syntax.applicative._
  val aSweetList = 2.pure[List]


  // Exercise
  import cats.syntax.functor._
//  def ap[F[_] : Applicative, A, B](ff: F[A => B])(fa: F[A]): F[B] = ff.map(f => ((a: A) => f(a))(a))
  def productWithApplicatives[F[_] : Applicative, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    Applicative[F].ap(fa.map(a => (b: B) => (a, b)))(fb)
  val listProduct = productWithApplicatives(sampleListNum, sampleListChar)

  def main(args: Array[String]): Unit = {
    println(listProduct)
  }
}
