package P2_AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}

object E15_MonadTransformers {

//  def sumAllOptions(list: List[Option[Int]]): Nothing = ???
  // here monad transformers can help us to unwrap all options and sum their inner value inside the list
  
  import cats.data.OptionT
  import cats.instances.list._
  val listNumberOptions: OptionT[List, Int] = OptionT(List(Option(34), Option(65)))
  val listCharacterOptions: OptionT[List, Char] = OptionT(List(Option('F'), Option('G')))
  val listTuples: OptionT[List, (Int, Char)] = for {
    char <- listCharacterOptions
    num <- listNumberOptions
  } yield (num, char)


  import cats.data.EitherT
  val listEither: EitherT[List, String, Int] = EitherT(List(Left("Wrong"), Right(34), Right(65)))
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureEither: EitherT[Future, String, Int] = EitherT.right(Future(45))


  val bandwidth: Map[String, Int] = Map(
    "server1" -> 200,
    "server2" -> 300,
    "server3" -> 175
  )
  type AsyncResponse[T] = EitherT[Future, String, T]
  def getBandwidth(server: String): AsyncResponse[Int] = bandwidth.get(server) match {
    case None => EitherT.left(Future("Server unreachable."))
    case Some(value) => EitherT.right(Future(value))
  }
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    bw1 <- getBandwidth(s1)
    bw2 <- getBandwidth(s2)
  } yield (bw1 + bw2) > 450
  def generateTrafficSurgeReport(s1: String, s2: String): AsyncResponse[String] = canWithstandSurge(s1, s2)
    .transform[String, String] {
      case Right(value1) =>
        if (value1) Right("Can withstand traffic.")
        else Left("Can't withstand traffic.\nCause: Bandwidth not enough.")
      case Left(value1) => Left("Can't withstand traffic.\nCause: " + value1)
  }


  def main(args: Array[String]): Unit = {
//    println(listTuples.value)

    println(generateTrafficSurgeReport("server1", "server2").value)
  }
}
