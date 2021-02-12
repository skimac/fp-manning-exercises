package FPManning.chapter4

trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(value)     =>  Right(f(value))
      case Left(exception)  =>  Left(exception)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(value)     =>  f(value)
      case Left(exception)  =>  Left(exception)
    }

  def orElse[EE >: E, B >: A](altValue: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(_) =>  this
      case Left(_)  =>  altValue
    }


  def map2[EE >: E, B, C](other: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      value       <-  this
      otherValue  <-  other
    } yield {
      f(value, otherValue)
    }
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]