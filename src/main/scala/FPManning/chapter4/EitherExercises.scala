package FPManning.chapter4

object EitherExercises extends App {

  def mean(numbers: IndexedSeq[Double]): Either[String, Double] =
    if (numbers.isEmpty)
      Left("Mean of an empty list!")
    else
      Right(numbers.sum / numbers.length)

  def safeDivision(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception =>  Left(e)
    }

  def safeDivision2(x: Int, y: Int): Either[Exception, Int] =
    Try(x / y)

  def Try[A](computation: => A): Either[Exception, A] =
    try Right(computation)
    catch {
      case e: Exception =>  Left(e)
    }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???


  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] =
    for {
      a         <-  Try(age.toInt)
      tickets   <-  Try(numberOfSpeedingTickets.toInt)
    } yield {
      insuranceRateQuote(a, tickets)
    }


  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) {
      Left("Name is empty.")
    } else {
      Right(new Name(name))
    }

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) {
      Left("Age is out of range")
    } else {
      Right(new Age(age))
    }

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age)) { (n, a) =>
      Person(n, a)
    }
}