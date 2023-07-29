package P2_AbstractMath

object E10_Functors {

  import cats.Functor
  import cats.instances.list._
  val listFunctor = Functor[List]


  // first implementation
  trait Tree[+T]
  case class Leaf[+T](value: T) extends Tree[T]
  case class Node[+T](value: T, right: Tree[T], left: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value) => Leaf(f(value))
      case Node(value, right, left) => Node(f(value), map(right)(f), map(left)(f))
    }
  }
  val sampleTree1: Node[Int] = Node(10, Leaf(5), Node(9, Leaf(3), Leaf(8)))

  // second implementation
  object Tree {
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def node[T](value: T, right: Tree[T], left: Tree[T]): Tree[T] = Node(value, right, left)
  }
  import Tree.{node, leaf}
  val sampleTree2: Tree[Int] = node(10, leaf(5), node(9, leaf(3), leaf(8)))


  import cats.syntax.functor._
  def increment2[T[_] : Functor](f: T[Int]): T[Int] = f.map(_ + 2)

  def main(args: Array[String]): Unit = {
//    println(Functor[Tree].map(sampleTree1)(_ + 3))
//    println(sampleTree2.map(_ + 3))
    println(increment2(sampleTree2))
  }
}
