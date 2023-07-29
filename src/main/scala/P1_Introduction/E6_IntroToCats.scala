package P1_Introduction

object E6_IntroToCats extends App {
  import cats.Eq

  import cats.instances.int._

  val intEquality: Eq[Int] = Eq[Int]
  val comparison = intEquality.eqv(3, 6)


  import cats.syntax.eq._
  val anotherComparison = 2 =!= 5


  case class Car(name: String, price: Double)
  implicit val toyCarEq: Eq[Car] = Eq.instance[Car] { (car1, car2) =>
    car1.price == car2.price
  }
}
