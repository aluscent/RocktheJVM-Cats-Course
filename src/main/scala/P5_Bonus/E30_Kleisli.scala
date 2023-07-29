package P5_Bonus

object E30_Kleisli {

  val fn1: Int => Option[String] = x => if (x % 2 == 0) Some(f"$x is even.") else None
  val fn2: Int => Option[Int] = x => Some(x + 6)
//  val fn3 = fn2 andThen fn1

  import cats.data.Kleisli
  val fn1K: Kleisli[Option, Int, String] = Kleisli(fn1)
  val fn2K: Kleisli[Option, Int, Int] = Kleisli(fn2)
  val fn3K: Kleisli[Option, Int, String] = fn2K andThen fn1K


  // Exercise
  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B]
  val times4 = Kleisli[Id, Int, Int](_ * 4)
  val add7 = Kleisli[Id, Int, Int](_ + 7)
  val combined = for {
    t4 <- times4
    a7 <- add7
  } yield t4 + a7

  def main(args: Array[String]): Unit = {

  }
}
