package FPManning.chapter4

trait Option[+A] {

//  def map[B](f: A => B): Option[B]
//  def flatMap[B](f: A => Option[B]): Option[B]
//  def getOrElse[B >: A](default: => B): B
//  def orElse[B >: A](ob: => Option[B]): Option[B]
//  def filter(f: A => Boolean): Option[A]

  def map[B](f: A => B): Option[B] = this match {
    case Some(get)  =>
      val mappedValue = f(get)
      Some(mappedValue)

    case _          =>  None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(get)  =>  f(get)
    case _          =>  None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get)  =>  get
    case None       =>  default
  }


  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None   =>  ob
    case _      =>  this
  }


  def filter(f: A => Boolean): Option[A] = this match {
    case Some(get)  if f(get) =>  this
    case _                      =>  None
  }
}

object Option {

  //Better name - mapLifted?
  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _.map(f)
  //    valOpt => valOpt.map(f)

  def map2[A,B,C](firstOpt: Option[A], secondOpt: Option[B])(f: (A, B) => C): Option[C] = {
    (firstOpt, secondOpt) match {
      case (Some(get), Some(get2))  =>  Some(f(get, get2))
      case _                        =>  None
    }
  }

  def map2Ans[A,B,C](firstOpt: Option[A], secondOpt: Option[B])(f: (A, B) => C): Option[C] =
//    for {
//      firstValue  <-  firstOpt
//      secondValue <-  secondOpt
//    } yield {
//      f(firstValue, secondValue)
//    }
    firstOpt.flatMap { firstValue =>
      secondOpt.map { secondValue =>
        f(firstValue, secondValue)
      }
    }

  def sequence[A](options: List[Option[A]]): Option[List[A]] =
    if (options.contains(None)) {
      None
    } else {
      val values = options.map {
        case Some(get)  =>  get
      }

      Some(values)
    }
}

case class Some[+A](get: A) extends Option[A] {
//
//  override def map[B](f: A => B): Option[B] = {
//    val mappedValue = f(get)
//    Some(mappedValue)
//  }
//
//  override def flatMap[B](f: A => Option[B]): Option[B] =
//    f(get)
//
//  override def getOrElse[B >: A](default: => B): B =
//    get
//
//  override def orElse[B >: A](ob: => Option[B]): Option[B] =
//    this
//
//  override def filter(f: A => Boolean): Option[A] =
//    if (f(get))
//      this
//    else
//      None
}

case object None extends Option[Nothing] {

//  override def map[B](f: Nothing => B): Option[B] =
//    None
//
//  override def flatMap[B](f: Nothing => Option[B]): Option[B] =
//    None
//
//  override def getOrElse[B >: Nothing](default: => B): B =
//    default
//
//  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] =
//    ob
//
//  override def filter(f: Nothing => Boolean): Option[Nothing] =
//    None
}