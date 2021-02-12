package FPManning.chapter5

object StreamsTest extends App {

  val numbersStream = Stream(1, 2, 3, 4, 5)
  val numbersList = numbersStream.toList
  val firstTwo = numbersStream.take(2).toList
  val withoutTwo = numbersStream.drop(2).toList
  val numbersTo3 = numbersStream.takeWhile(_ <= 3).toList
  val threeExists = numbersStream.exists(_ == 3)
  val allPositive = numbersStream.forAll(_ > 0)
  val allNegative = numbersStream.forAll(_ < 0)
  val allGreaterThan2 = numbersStream.forAll(_ > 2)
  val numbersTo4 = numbersStream.takeWhile2(_ <= 4).toList
  val numbersToEven = numbersStream.takeWhile2(_ % 2 != 0).toList
  val numbersToOdd = numbersStream.takeWhile2(_ % 2 == 0).toList
  val headOpt = numbersStream.headOption2
  val evenNumbers = numbersStream.filter(_ % 2 == 0).toList
  val oddNumbers = numbersStream.filter(_ % 2 != 0).toList
  val appended = numbersStream.append2(Stream(7, 13, 20)).toList
  val squares = numbersStream.map(x => x * x).toList
  val evenSquares = numbersStream.map { nr =>
    nr * nr
  }.filter { x =>
    x % 2 == 0
  }.toList
  val oddSquares = numbersStream.map { nr =>
    nr * nr
  }.filter { x =>
    x % 2 != 0
  }.toList
  val sumElements = Stream(10, 20, 30, 40, 50)
  val sums = numbersStream.flatMap { number =>
    sumElements.map { element =>
      number + element
    }
  }.toList

  println(
    s"""
      | Numbers: $numbersList
      | First two: $firstTwo
      | Without two: $withoutTwo
      | To 3: $numbersTo3
      | 3 exists: $threeExists
      | All positive: $allPositive
      | All negative: $allNegative
      | All greater than 2: $allGreaterThan2
      | To 4: $numbersTo4
      | To even: $numbersToEven
      | To odd: $numbersToOdd
      | Head option: $headOpt
      | Even numbers: $evenNumbers
      | Odd numbers: $oddNumbers
      | Appended numbers: $appended
      | Squares: $squares
      | Even squares: $evenSquares
      | Odd squares: $oddSquares
      | Find 2: ${numbersStream.find(_ == 2)}
      | Find divisable by 4: ${numbersStream.find(_ % 4 == 0)}
      | Find divisable by 8: ${numbersStream.find(_ % 8 == 0)}
      | Sums: $sums
      |""".stripMargin)
}