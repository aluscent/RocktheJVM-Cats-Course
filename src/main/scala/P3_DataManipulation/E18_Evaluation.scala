package P3_DataManipulation

object E18_Evaluation {

  /**
   * Evaluations in Cats:
   *  - Eagerly
   *  - Lazily everytime requested
   *  - Lazily with memoization
   */

  import cats.Eval

  val instantEval: Eval[Int] = Eval.now {
    println("instant eval")
    3254
  }
  val lazyEval: Eval[Int] = Eval.always {
    println("lazy eval")
    9348
  }
  val memoizationEval: Eval[Int] = Eval.later {
    println("memoized eval")
    3497
  }

  val composedEval1: Eval[Int] = instantEval.flatMap(x => memoizationEval.map(y => x + y))
  val composedEval2: Eval[Int] = for {
    a <- instantEval
    b <- memoizationEval
    c <- lazyEval
    d <- instantEval
    e <- lazyEval
  } yield a + b + c + d + e

  val dontRedo: Eval[Int] = composedEval2.memoize
  val tutorial: Eval[String] = Eval.always {
    println("Step 1")
    "Put the guitar on your lap"
  }.map { step1 =>
    println("Step 2")
    s"$step1 then put your hand on the neck"
  }.memoize.map { step12 =>
    println("Step 3")
    s"$step12 then with your right hand strike the string."
  }


  // Exercise
  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(())
    .flatMap(_ => eval)


  // Exercise
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else defer(reverseEval(list.tail) map (_ :+ list.head))

  def main(args: Array[String]): Unit = {
    println(reverseEval((500 to 150000).toList).value)
  }
}
