package P4_TypeClasses

import cats.kernel.Monoid

object E27_Folding {

  object ListExercise {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldLeft(List.empty[B])((a, b) => f(b) :: a)

    def flatMap[A, B](list: List[A])(fb: A => List[B]): List[B] =
      list.foldLeft(List.empty[B])((a, b) => fb(b) ++ a)

    def filter[A](list: List[A])(f: A => Boolean): List[A] =
      list.foldLeft(List.empty[A])((a, b) => if (f(b)) b :: a else a)

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)((a, b) => monoid.combine(a, b))
  }

  import cats.Foldable
  Foldable[List].foldLeft(List(1,3,7), 0)(_ + _)
  Foldable[Option].foldLeft(Option(3), 5)(_ * _)

  // nesting
  val combined = (Foldable[List] compose Foldable[Vector]) combineAll List(Vector(4,5,3), Vector(5,7,3))

  // extension methods
  import cats.syntax.foldable._
  val sumThree: Int = List(1,2,3).combineAll
  
}
