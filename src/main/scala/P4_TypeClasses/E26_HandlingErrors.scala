package P4_TypeClasses

import cats.Monad

object E26_HandlingErrors {
  trait MyMonadError[T[_], E] extends Monad[T] {
    def raiseError[A](e: E): T[A]
  }

  import cats.MonadError
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]
  val success: ErrorOr[Int] = monadErrorEither.pure(32) // Either[String, Int] == Right(32)
  val failure: ErrorOr[Int] = monadErrorEither.raiseError[Int]("Something wrong.") // Either[String, Int] == Left("Something wrong.")
  // recover
  val handledMonad: ErrorOr[Int] = monadErrorEither
    .handleError(failure) {
    case "Something wrong." => 54
    case _ => 11
  }
  // recoverWith
  val handledMonad2: ErrorOr[Int] = monadErrorEither
    .handleErrorWith(failure) {
      case "Something wrong." => monadErrorEither.pure(54)
      case _ => Left("11")
    }
  // filter
  val filteredSuccess: ErrorOr[Int] = monadErrorEither
    .ensure(failure)("Something wrong.")(_ > 100)

  def main(args: Array[String]): Unit = {

  }
}
