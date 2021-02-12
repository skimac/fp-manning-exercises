package FPManning.chapter4

import FPManning.chapter4.Option.{lift, map2}


object OptionExercises extends App {

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val ageOpt: Option[Int] = Try(age.toInt)
    val numberOpt: Option[Int] = Try(numberOfSpeedingTickets.toInt)

    map2(ageOpt, numberOpt)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case _: Exception =>  None }

  val abs0: Option[Double] => Option[Double] =
    lift(math.abs)

  val abs1: Option[Double] => Option[Double] =
    _.map(math.abs)

  val optNumbers = Seq(Some(1.0), None, Some(-2.5))

  optNumbers.foreach { numberOpt =>
    println(
      s"""
        | Number: $numberOpt
        | Abs0: ${abs0(numberOpt)}
        | Abs1: ${abs1(numberOpt)}
        |""".stripMargin)
  }
}