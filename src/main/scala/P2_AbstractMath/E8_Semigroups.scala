package P2_AbstractMath

import cats.{Eq, Monoid}

import scala.util.Random

object E8_Semigroups {

  /**
   * Semigroups combine elements of the same type
   */
  import cats.Semigroup

  import cats.instances.int._
  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 4) // addition

  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination: String = naturalStringSemigroup.combine("hello ", "world") // concatenation


  trait Log

  case class Expense(id: Long, amount: Double) extends Log
  class ExpenseMonoid extends Monoid[Expense] { self =>
    override def empty: Expense = Expense(0, 0)

    override def combine(x: Expense, y: Expense): Expense = Expense(math.max(x.id, y.id) + 1, x.amount + y.amount)
  }
  implicit val catsKernelExpenseMonoid: ExpenseMonoid = new ExpenseMonoid
  implicit def expenseEquality: Eq[Expense] = Eq.instance[Expense](_.id == _.id)
  def expenseCombination: Semigroup[Expense] = Semigroup[Expense]


  case class Income(id: Long, amount: Double) extends Log
  def incomeCombination: Semigroup[Income] = Semigroup.instance[Income] {
    (x, y) => Income(math.max(x.id, y.id) + 1, x.amount + y.amount)
  }




  def main(args: Array[String]): Unit = {
    val sampleExpenses = (1 to 10).toList.map(x => Expense(x, Random.between(100, 300)))

    import cats.syntax.semigroup._
    println(sampleExpenses.reduce(_ |+| _))
//    println(sampleExpenses[Income].reduce(_ |+| _))
  }
}
