package P3_DataManipulation

import scala.annotation.tailrec

object E20_DataValidation1 {

  import cats.data.Validated
  val valid1: Validated[String, Int] = Validated.Valid(34) // Right(value)
  val invalid1: Validated[String, Int] = Validated.Invalid("Invalid") // Left(value)

  val test = Validated.cond(90 > 20, 90, "Invalid")

  def checkPrime(num: Int): Boolean = {
    @tailrec
    def tailRecPrime(d: Int): Boolean =
      if (d <= 0) false
      else if (d < 2) true
      else if (num % d == 0 ) false
      else tailRecPrime(d - 1)

    tailRecPrime(num - 1)
  }

  def testNum(num: Int): Either[List[String], Int] = {
    var flag = true
    var listStatuses = List[String]()

    if (!checkPrime(num)) {
      listStatuses = "Not prime" :: listStatuses
      flag = false
    }
    if (num < 0) {
      listStatuses = "Negative" :: listStatuses
      flag = false
    }
    if (num % 2 != 0) {
      listStatuses = "Not even" :: listStatuses
      flag = false
    }
    if (num > 100) {
      listStatuses = "Greater than 100" :: listStatuses
      flag = false
    }

    if (flag) Right(num)
    else Left(listStatuses)
  }


  def validatedNum(num: Int): Validated[List[String], Int] =
    Validated.cond(checkPrime(num), num, List("Not prime")) combine
      Validated.cond(num % 2 != 0, num, List("Not even")) combine
      Validated.cond(num < 0, num, List("Negative")) combine
      Validated.cond(num > 100, num, List("Greater than 100"))


  // Part 2
  // chaining
  valid1.andThen(v => invalid1)
  valid1.ensure(List("Something wrong"))(_ % 2 == 0)

  val eitherToValidated: Validated[String, Int] = Validated.fromEither(Right(33))
  val optionToValidated: Validated[String, Int] = Validated.fromOption(Some(90), "Nothing")
  val validatedToEither: Either[String, Int] = valid1.toEither


  // Exercise
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"$fieldName is not provided."))

    def nonBlank(value: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"Blank field."))

    def emailValid(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("Email not valid."))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("Password simple."))

    def validateForm(form: Map[String, String]): FormValidation[String] =
      getValue(form, "Name").andThen(nonBlank)
        .combine(getValue(form, "Password").andThen(passwordCheck))
        .combine(getValue(form, "Email").andThen(emailValid))
        .map(_ => "Success.")
  }


  import cats.syntax.validated._
  val validInt: Validated[List[String], Int] = 43.valid[List[String]]
  val error: Validated[String, Int] = "Something wrong.".invalid[Int]

  def main(args: Array[String]): Unit = {
//    (1 to 10).toList.foreach(x => testNum(x) match {
//      case Right(value) => println(value)
//      case Left(value) => println(value)
//    })

//    (1 to 10).toList.foreach(x => println(validatedNum(x)))

    println(FormValidation.validateForm(Map(
      "Name" -> "",
      "Email" -> "Alioutlook.com",
      "Password" -> "Aa@123456789"
    )))
  }
}
