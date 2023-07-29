package P3_DataManipulation

import cats.Id

object E16_Readers {

  case class Configuration(dbUser: String, password: String, host: String, port: Int, emailReplyTo: String)
  case class DBConnection(user: String, pass: String) {
    def getStatus(orderId: Long): String = "???"
    def getLastOrderId(user: String): Long = 3865567567L
  }
  case class HTTPService(host: String, port: Int) {
    def start(): Unit = println("Server started.")
  }

  val config: Configuration = Configuration("Ali", "12345", "localhost", 12345, "ali@gmail.com")
  // cats reader
  import cats.data.Reader
  val dbReader: Reader[Configuration, DBConnection] = Reader(conf => DBConnection(conf.dbUser, conf.password))
  dbReader.run(config)

  val orderStatusReader: Reader[Configuration, String] = dbReader.map(_.getStatus(123))
  orderStatusReader.run(config)

  def getLastOrderStatus(user: String): Reader[Configuration, String] = dbReader
    .map(_.getLastOrderId(user))
    .flatMap(x => dbReader.map(_.getStatus(x)))


  // Exercise
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"Sending email to $address containing: $contents"
  }

  def emailUser(user: String, email: String): String = {
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))

    val emailReader = for {
      orderId <- dbReader.map(_.getLastOrderId(user))
      status <- dbReader.map(_.getStatus(orderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(email, s"The email status is $status")

    emailReader.run(config)
  }

  /**
   * This pattern is pure functional dependency injection.
   */

  def main(args: Array[String]): Unit = {
    println(emailUser("Ali", "sana@gmail.com"))
  }
}
