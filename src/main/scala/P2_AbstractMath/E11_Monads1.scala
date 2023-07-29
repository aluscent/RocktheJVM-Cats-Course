package P2_AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object E11_Monads1 {

  val optionN: Option[Int] = Option(2)
  val optionC: Option[Char] = Option('H')
  val combination: Option[(Int, Char)] = for {
    n <- optionN
    c <- optionC
  } yield (n, c)


  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureN: Future[Int] = Future(2)
  val futureC: Future[Char] = Future('H')
  for {
    n <- futureN
    c <- futureC
  } yield (n, c)


  import cats.Monad
  import cats.instances.option._
  val optionMonad: cats.Monad[Option] = Monad[Option]
  val anOption: Option[Int] = optionMonad.pure(4)
  val transformedOption: Option[Int] = optionMonad.flatMap(anOption)(x => Option(x + 2))

  import cats.syntax.monad._
  def getCombination[T[_] : Monad, A, B](ta: T[A], tb: T[B])(implicit monad: Monad[T]): T[(A, B)] =
    monad.flatMap(ta)(a => monad.map(tb)(b => (a, b)))

  // extension methods
  import cats.syntax.applicative._ // pure is here
  val one = 1.pure[Option]

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def getCombination2[T[_] : Monad, A, B](ta: T[A], tb: T[B]): T[(A, B)] =
    ta.flatMap(a => tb.map(b => (a, b)))

  trait MyMonad[T[_]] {
    def pure[A](value: A): T[A]
    def flatMap[A, B](ta: T[A])(f: A => T[B]): T[B]
    def map[A, B](ta: T[A])(f: A => B): T[B] = flatMap(ta)(x => pure(f(x)))
  }


  def main(args: Array[String]): Unit = {

  }
}
