package P2_AbstractMath

import cats.Monoid
import cats.instances.int._
import java.time.LocalDateTime
import scala.util.Random

object E9_Monoids {

  def recentLocalDateTime(x: LocalDateTime, y: LocalDateTime): LocalDateTime = if (x.isAfter(y)) x else y



  case class Income(id: Long, amount: Double)
  class IncomeMonoid extends Monoid[Income] {
    override def empty: Income = Income(0, 0)

    override def combine(x: Income, y: Income): Income = Income(x.id + 1, x.amount + y.amount)
  }
  implicit def catsIncomeMonoid = new IncomeMonoid

  val samples: List[Income] = (1 to 10).toList.map(x => Income(x, Random.between(100, 300)))


  // general reducer
//  def reduceByFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
//    list.foldRight(new T)(_ |+| _)

  def combineByFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldRight(monoid.empty)(monoid.combine)


  val phonebook: List[Map[String, Int]] = List(
    Map("Alice" -> 123, "Bob" -> 354),
    Map("Charlie" -> 345, "Dave" -> 233),
    Map("Emma" -> 235)
  )
  def combinePhonebook(list: List[Map[String, Int]])(implicit monoid: Monoid[Map[String, Int]]): Map[String, Int] =
//    list.foldRight(monoid.empty)(monoid.combine)
    combineByFold(list)


  case class Item(name: String, price: Double, creationDate: LocalDateTime)
  case class ShoppingCart(items: List[Item], total: Double, orderDate: LocalDateTime)
  class ShoppingCartMonoid extends Monoid[ShoppingCart] {
    override def empty: ShoppingCart = ShoppingCart(List.empty[Item], 0, LocalDateTime.MIN)

    override def combine(x: ShoppingCart, y: ShoppingCart): ShoppingCart =
      ShoppingCart(x.items ++ y.items, x.total + y.total, recentLocalDateTime(x.orderDate, y.orderDate))
  }
  implicit val catsMonoidShoppingCart: ShoppingCartMonoid = new ShoppingCartMonoid
  def checkout(list: List[ShoppingCart])(implicit monoid: Monoid[ShoppingCart]): ShoppingCart =
    combineByFold(list)

  // sample shopping carts
  val sampleShoppingCarts: List[ShoppingCart] = List(
    ShoppingCart(List(Item("mouse", 10, LocalDateTime.of(2022,6,23,10,57))), 10, LocalDateTime.of(2022,10,23,10,57)),
    ShoppingCart(List(Item("monitor", 50, LocalDateTime.of(2021,6,23,13,0))), 50, LocalDateTime.of(2022,10,23,11,13))
  )


  def main(args: Array[String]): Unit = {
//    println(combineByFold(samples))
//    println(combinePhonebook(phonebook))
    println(checkout(sampleShoppingCarts))
  }
}
