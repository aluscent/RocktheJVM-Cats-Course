package P4_TypeClasses

import cats.Monad

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object E22_Semigroupal {

  trait MySermigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  val optionSG: Semigroupal[Option] = Semigroupal[Option]
  val tupledOption: Option[(Int, Int)] = optionSG.product(Option(3), Option(8)) // Some((3,8))
  val tupledOption2 = optionSG.product(Some(98), None) // None

  implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
  val futureSG = Semigroupal[Future]
  val tupledFuture = futureSG.product(Future("hello"), Future(54))

  val tupledList = Semigroupal[List].product(List(1,2), List("hello")) // cartesian product of lists


  // Exercise
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  val tupledList2: List[(Int, String)] = productWithMonads(List(1,2), List("hello"))


  import cats.data.Validated
  type ErrorOr[T] = Validated[List[String], T]
  val validatedSG: Semigroupal[ErrorOr] = Semigroupal[ErrorOr]
  val tupledValidated = validatedSG.product(Validated.Invalid(List("Error")), Validated.valid(23))


  type EitherErrorOr[T] = Either[List[String], T]
  val eitherSG = Semigroupal[EitherErrorOr]
  val tupledEither = eitherSG.product(Left(List("Error")), Right(23))


  // Exercise
  

  def main(args: Array[String]): Unit = {
    Thread.sleep(100)
    println(tupledEither)
  }
}
