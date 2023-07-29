package P4_TypeClasses

import cats.data.Validated
import cats.{Applicative, Foldable, Monad, data}
import cats.implicits._

import java.util.concurrent.Executors
import scala.collection.BuildFrom
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}

object E28_Traverse1 {

  implicit val ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  private val servers: List[String] = List("server-ci", "server-stg", "server-prod")
  def getBandwidth(hostname: String): Future[Int] =
    Future(hostname.length * 10)

  val allBandwidth1: Future[List[Int]] =
    servers.foldLeft(Future(List.empty[Int])) { (a, b) =>
      for {
        accBand <- a
        band <- getBandwidth(b)
      } yield band :: accBand
    }

  val allBandwidth2: Future[List[Int]] = Future.traverse(servers)(getBandwidth)

  val allBandwidth3: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))


  // Exercise
  import cats.syntax.flatMap._
  import cats.syntax.applicative._
  import cats.syntax.functor._
  private def listTraverse1[F[_] : Monad, A, B](list: List[A])(fn: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F]) { (l, a) =>
      for {
        l_ <- l
        b <- fn(a)
      } yield b :: l_
    }
  }

  private def listTraverse2[F[_] : Applicative, A, B](list: List[A])(fn: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F]) { (l, a) =>
      (l, fn(a)).mapN((c, b) => b :: c)
    }
  }

  def listSequence1[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse2(list)(identity)


  import cats.instances.vector._
  private val ls1 = listSequence1(List(Vector(3,9), Vector(4,6), Vector(6,4,8)))(catsStdInstancesForVector)


  // Exercise
  def filterAsOption(list: List[Int], predicate: Int => Boolean): Option[List[Int]] =
    listTraverse2[Option, Int, Int](list)(x => Some(x).filter(predicate))
  val test1 = filterAsOption(List(4,6,3), _ % 2 == 0) // None

  type ErrorOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int], predicate: Int => Boolean): ErrorOr[List[Int]] =
    listTraverse2[ErrorOr, Int, Int](list) { x =>
      if (predicate(x)) Validated.valid(x)
      else Validated.invalid(List(f"$x is invalid."))
    }
  val test2 = filterAsValidated(List(4,6,3), _ % 2 == 0)

  trait MyTraverse[L[_]] extends Foldable[L] {
    def traverse[F[_] : Applicative, A, B](container: L[A])(fn: A => F[B]): F[L[B]]
    def sequence[F[_] : Applicative, A](container: L[F[A]]): F[L[A]] =
      traverse(container)(identity)

    type Identity[T] = T
    def map[A, B](container: L[A])(fn: A => B): L[B] =
      traverse[Identity, A, B](container)(fn)
  }

  def main(args: Array[String]): Unit = {
    val l = List(40,60,30)
    val f = listTraverse2(l)(a => Future(a + 1))
    Thread.sleep(100)
    println(ls1)
  }
}
