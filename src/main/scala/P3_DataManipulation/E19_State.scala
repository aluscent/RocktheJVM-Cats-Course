package P3_DataManipulation

object E19_State {

  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State(x => (x, s"Current count is $x"))

  val (eleven, log10) = countAndSay.run(10).value


  // Exercise
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State(state => (ShoppingCart(state.items :+ item, state.total + price), state.total + price))


  // Exercise
  def inspect[A, B](f: A => B): State[A, B] = State(state => (state, f(state)))
  def get[A]: State[A, A] = State(state => (state, state))
  def set[A](value: A): State[A, Unit] = State(state => (value, ()))
  def modify[A](f: A => A): State[A, Unit] = State(state => (f(state), ()))


  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](5 + a)
    b <- get[Int]
    _ <- modify[Int](_ + 33)
    c <- inspect[Int, Int](_ - 23)
  } yield (a, b, c)



  def main(args: Array[String]): Unit = {
    val totCart = for {
      _ <- addToCart("meat", 10)
      _ <- addToCart("potato", 2)
      cart <- addToCart("phone", 300)
    } yield cart

    println(totCart.run(ShoppingCart(List.empty, 0)).value._1)
  }
}
