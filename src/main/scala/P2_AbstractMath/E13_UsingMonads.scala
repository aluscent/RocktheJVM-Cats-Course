package P2_AbstractMath

import scala.util.{Failure, Success, Try}

object E13_UsingMonads {
  import cats.Monad

  val anEither: Either[String, Int] = Right(42)
  val otherEither: Either[String, Int] = Left("hi")

  type ErrorOr[T] = Either[Throwable, T] // the left argument is undesirable, and right argument is desirable
  import cats.instances.either._
  val errorMonad: Monad[ErrorOr] = Monad[ErrorOr]
  val check: ErrorOr[Int] = errorMonad.pure(22)


  // an online store
  type LoadingOr[T] = Either[String, T]
  case class OrderStatus(id: Long, status: String)
//  def getOrderStatus(id: Long): LoadingOr[OrderStatus] = ???
//  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] = ???
//  val location: Either[String, String] = getOrderStatus(457L).flatMap(x => trackLocation(x))


  case class Connection(host: String, port: String)
  trait HttpService[M[_]] {
    def getConnection(config: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }
  val config: Map[String, String] = Map("host" -> "localhost", "port" -> "12345")

  import cats.syntax.applicative._
  class TryHttpService extends HttpService[Try] {
    override def getConnection(config: Map[String, String]): Try[Connection] =
        Try(Connection(config("host"), config("port")))

    override def issueRequest(connection: Connection, payload: String): Try[String] =
      if (payload.length < 20) Failure(new Throwable) else Success("Payload accepted")
  }

  val reponseTry: Try[String] = (new TryHttpService).getConnection(config)
    .flatMap(conn => (new TryHttpService).issueRequest(conn, "This is a test Try! Let's catch up."))


  class ListHttpService extends HttpService[List] {
    override def getConnection(config: Map[String, String]): List[Connection] =
      if (config.contains("host") && config.contains("port")) List(Connection(config("host"), config("port")))
      else List.empty

    override def issueRequest(connection: Connection, payload: String): List[String] =
      if (payload.length < 20) List.empty else List("Payload accepted")
  }

  val responseList: List[String] = (new ListHttpService).getConnection(config)
    .flatMap(conn => (new ListHttpService).issueRequest(conn, "This is a test Try! Let's catch up."))

  class ErrorOrHttpService extends HttpService[ErrorOr] {
    override def getConnection(config: Map[String, String]): ErrorOr[Connection] =
      if (config.contains("host") && config.contains("port")) Right(Connection(config("host"), config("port")))
      else Left(new RuntimeException)

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length < 20) Left(new Throwable) else Right("Payload accepted")
  }

  val responseErrorOr: Either[Throwable, String] = (new ErrorOrHttpService).getConnection(config)
    .flatMap(conn => (new ErrorOrHttpService).issueRequest(conn, "This is a test Try! Let's catch up."))

  class OptionHttpService extends HttpService[Option] {
    override def getConnection(config: Map[String, String]): Option[Connection] =
      if (config.contains("host") && config.contains("port")) Some(Connection(config("host"), config("port"))) else None

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length < 20) None else Some("Payload accepted")
  }

  import cats.syntax.flatMap._
  def getResponse[M[_] : Monad](service: HttpService[M], payload: String): M[String] =
    service.getConnection(config).flatMap(conn => service.issueRequest(conn, payload))

  def main(args: Array[String]): Unit = {
    println(getResponse(new ErrorOrHttpService, "This is a test Try! Let's catch up."))
    println(getResponse(new TryHttpService, "This is a test Try! Let's catch up."))
  }
}
