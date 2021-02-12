package FPManning.chapter5

object InfiniteStreams extends App {

  val ones: Stream[Int] = Stream.cons(1, ones)
  val ones2: Stream[Int] = Stream.unfold(1) { a =>
    Some(a, a)
  }
  val twos = Stream.constant(2)
  val threes = Stream.constant2(3)
  val incrementalNumbers = Stream.from(10)
  val incremental2 = Stream.from2(10)
  val fibs = Stream.fibs().take(1000)
  val fibs2 = Stream.fibs2().take(1000)
  val unfolded = Stream.unfold(233) { a =>
    if (a > 0) {
      Some(a, a - 50)
    } else {
      None
    }
  }
  val numbers = Stream(1, 2, 3, 4, 5)
  val squares = numbers.map2(x => x * x)
  val numbersTo3 = numbers.takeWhileUnfold(_ <= 3)
  val numbersWhileEven = numbers.takeWhileUnfold(_ % 2 == 0)
  val numbersWhileOdd = numbers.takeWhileUnfold(_ % 2 != 0)
  val letters = Stream("A", "B", "C", "D", "E")
  val numbersWithLetters = numbers.zipWith(letters)
  val letters2 = letters.append2(Stream("F", "G", "H"))
  val numbersWithLetters2 = numbers.zipWith(letters2)
  val numbers2 = numbers.append2(Stream(6, 7, 8, 9))
  val numbersWithLetters3 = numbers2.zipWith(letters)
  val numbersWithLetters4 = numbers2.zipWith(letters2)
  val zipAll1 = numbers.zipAll(letters)
  val zipAll2 = numbers.zipAll(letters2)
  val zipAll3 = numbers2.zipAll(letters)
  val zipAll4 = numbers2.zipAll(letters2)

  println(
    s"""
      | Ones: ${ones.take(1000).toList}
      | Ones (with unfold): ${ones2.take(1000).toList}
      | Twos: ${twos.take(1000).toList}
      | Threes (with unfold): ${threes.take(1000).toList}
      | Incremental from 10: ${incrementalNumbers.take(1000).toList}
      | Incremental from 10 (with unfold): ${incremental2.take(1000).toList}
      | Odd exists: ${ones.exists(_ % 2 != 0)}
      | Fibs: ${fibs.toList}
      | Fibs2: ${fibs.take2Ans(1000).toList}
      | Unfolded: ${unfolded.take2(4).toList}
      | Squares (with unfold): ${squares.toList}
      | Take while x <= 3 (with unfold): ${numbersTo3.toList}
      | Take while even (with unfold): ${numbersWhileEven.toList}
      | Take while odd (with unfold): ${numbersWhileOdd.toList}
      | Numbers zipped with letters: ${numbersWithLetters.toList}
      | Numbers with letters 2: ${numbersWithLetters2.toList}
      | Numbers with letters 3: ${numbersWithLetters3.toList}
      | Numbers with letters 4: ${numbersWithLetters4.toList}
      | Zip all 1: ${zipAll1.toList}
      | Zip all 2: ${zipAll2.toList}
      | Zip all 3: ${zipAll3.toList}
      | Zip all 4: ${zipAll4.toList}
      |""".stripMargin)

  //Never terminates because 'ones' are infinite. Results in StackOverFlowError
//  println(s"Take while 1: ${ones.takeWhile2(number => number == 1).toList}")
//  println(s"All ones: ${ones.forAll2(_ == 1)}")
}