package P2_AbstractMath

import P2_AbstractMath.E14_CustomMonads.Tree.node

import scala.annotation.tailrec

object E14_CustomMonads {

  import cats.Monad

  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(value)) => tailRecM(value)(f)
      case Some(Right(value)) => Some(value)
    }
  }


  type Identity[T] = T
  val aNumber: Identity[Int] = 42
  implicit object IdentityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(value) => tailRecM(value)(f)
      case Right(value) => value
    }
  }


  sealed trait Tree[+T] {
    def inValue: T
    def merge[A >: T](left: Tree[A], right: Tree[A]): Tree[A]
  }
  final case class Leaf[+T](value: T) extends Tree[T] {
    override def inValue = value

    override def merge[A >: T](left: Tree[A], right: Tree[A]): Tree[A] = node(value, left, right)
  }
  final case class Node[+T](value: T, Left: Tree[T], Right: Tree[T]) extends Tree[T] {
    override def inValue: T = value

    override def merge[A >: T](left: Tree[A], right: Tree[A]): Tree[A] = this
  }
  object Tree {
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def node[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Node(value, left, right)
  }
  implicit object TreeMonad extends Monad[Tree] {
    import Tree._
    override def pure[A](x: A): Tree[A] = leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value) => f(value)
      case Node(value, left, right) => f(value) match {
        case Leaf(v) => node(v, flatMap(left)(f), flatMap(right)(f))
        case Node(v, innerLeft, innerRight) => node(v, flatMap(left)(f), flatMap(right)(f))
      }
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]) : Tree[B] = t match {
        case Leaf(Left(value)) => stackRec(f(value))
        case Leaf(Right(value)) => leaf(value)
        case Node(value, left, right) => value match {
          case Left(value1) => stackRec(f(value1)).merge(stackRec(left), stackRec(right))
          case Right(value1) => node(value1, stackRec(left), stackRec(right))
        }
      }

      def tailRec(toDo: List[Tree[Either[A, B]]], expanded: Set[Tree[Either[A, B]]], done: List[Tree[B]]): Tree[B] = {
        if (toDo.isEmpty) done.head
        else toDo.head match {
          case Leaf(Left(value)) => tailRec(f(value) :: toDo.tail, expanded, done)
          case Leaf(Right(value)) => tailRec(toDo.tail, expanded, leaf(value) :: done)
//          case node @ Node(value, left, right) =>
//            if (!expanded.contains(node)) tailRec(left :: right :: toDo, expanded + node, done)
//            else
        }
      }

      stackRec(f(a))
    }
  }

  import Tree._
  val sampleTree: Tree[Int] = node(10, node(7, leaf(5), node(3, leaf(5), leaf(8))), leaf(0))


  import cats.syntax.flatMap._
  def main(args: Array[String]): Unit = {
    println(sampleTree.flatMap(x => node(x, leaf(0), leaf(0))))
  }
}
