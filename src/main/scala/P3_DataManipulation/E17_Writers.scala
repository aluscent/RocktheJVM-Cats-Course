package P3_DataManipulation

import cats.Id
import cats.data.{Writer, WriterT}

object E17_Writers {

  // 1- define at the start
  val aWriter: Writer[List[String], Int] = Writer(List("hello","world"), 43)
  // 2- modify with pure FP
  val valueIncreaseLogsSame: WriterT[Id, List[String], Int] = aWriter.map(_ + 1)
  val valueSameLogsChange: WriterT[Id, List[String], Int] = aWriter.mapWritten(_ :+ " world!")
  val bothChange_1: WriterT[Id, List[String], Int] = aWriter.bimap(_ :+ "Ali!", _ * 2)
  val bothChange_2: WriterT[Id, List[String], Int] = aWriter.mapBoth((list, int) => (list :+ "Sana", int / 5))
  // 3- dump either value or logs
  val desiredValue: Id[Int] = aWriter.value
  val logs: Id[List[String]] = aWriter.written
  val (l, v) = aWriter.run


  val writerA: WriterT[Id, Option[String], Int] = Writer(Option("Log A"), 10)
  val writerB: WriterT[Id, Option[String], Int] = Writer(Option("Log B"), 15)
  val compositeWriter: WriterT[Id, Option[String], Int] = for {
    a <- writerA
    b <- writerB
  } yield a + b


  // Exercise
  def countAndSay(n: Int): Unit = n match {
    case n if n <= 0 => println("Starting...")
    case _ =>
      countAndSay(n - 1)
      println(n)
  }
  def countAndLog(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector("Starting..."), 0)
    else for {
      a <- Writer(Vector.empty, 0) mapBoth {(_, _) => (Vector(n.toString), n)}
      b <- countAndLog(n - 1)
    } yield b + a

  def logSum(n: Int): Writer[Vector[(String,String)], Int] = {
    if (n <= 0) Writer(Vector.empty, 0)
    else logSum(n - 1).mapBoth { (log, value) =>
      (log :+ (s"Now at $n", s"Sum is $value"), value + n)
    }
  }

  def main(args: Array[String]): Unit = {
    val temp = logSum(6).written
    println(temp.foreach(x => println(x._1 + "\t" + x._2)))
  }
}
