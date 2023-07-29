package P1_Introduction

import cats.Eq
import cats.syntax.eq._
import cats.instances.int._
import cats.instances.option._

object E7_TCVariance {

  class Animal
  class Cat extends Animal
  class Garfield extends Cat

  // covariant type: subtyping is propagated through the generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat]
  /** I want a cage for all types of animals, it gives me a cage for cat that can contain any animal. */

  // contravariant type: subtyping is propagated BACKWARDS through the generic type
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal]
  /** I want a vet for a cat, it gives me a vet for all types of animals, including cat. */

  /**
   * As a rule of thumb, if a class:
   *  1) Has/Contains a T, then it is a "Covariant". Just like a List, Option, Cage, etc.
   *  2) Acts on a T, then it is a "Contravariant". Just like a Vet, Function, etc.
   *
   *  Variances affect how the type classes are being fetched.
   */
  trait SoundMaker[-T] // looks like acts on T
  implicit object CatSoundMaker extends SoundMaker[Cat]
  def makeSound[T](implicit maker: SoundMaker[T]): Unit = println("Wow!")
  //makeSound[Animal]
  makeSound[Cat]
  makeSound[Garfield]

  /*
  rule 1: contravariant types can use their supertypes if nothing is available strictly for them
  rule 2: covariant type classes will always stick with more specific type class instance.
    However it can confuse compiler if the more general type class is also present.
  rule 3: you can't have both benefits!
   */


  trait Show[+T] {
    def show: Unit
  }
  implicit object AnimalShow extends Show[Animal] {
    override def show: Unit = println("Animal show!")
  }
  implicit object CatShow extends Show[Cat] {
    override def show: Unit = println("Cat show!!")
  }

  def organizedShow[T](implicit event: Show[T]): Unit = event.show

  def main(args: Array[String]): Unit = {
//    organizedShow[Garfield]
    organizedShow[Cat]
//    organizedShow[Animal]
  }

  /**
   * Cats uses Invariant type classes.
   */
}
