package FPManning.chapter5

import FPManning.chapter5.Stream.{cons, empty, unfold}

import scala.annotation.tailrec


sealed trait Stream[+A] {

  def headOption: Option[A] =
    this match {
      case Empty           =>  None
      case Cons(head, _)   =>  Some(head())
    }

  def headOption2: Option[A] =
    foldRight(None: Option[A]) { (head, _) =>
      Some(head)
    }

  def toList: List[A] = {
    this match {
      case Empty        =>  Nil

      case Cons(h, t)   =>
        lazy val head = h()
        lazy val tail = t()
        head :: tail.toList
    }
  }

  def toListAns: List[A] = {

    @tailrec
    def go(stream: Stream[A], acc: List[A]): List[A] =
      stream match {
        case Cons(h, t) =>  go(t(), h() :: acc)
        case _          =>  acc
      }

    go(this, List().reverse)
  }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0  =>
        lazy val tail = t()
        cons(h(), tail.take(n - 1))

      case _  =>
        empty()
    }

  def take2(n: Int): Stream[A] =
    unfold(this) {
      case Cons(h, t) if n > 0  =>  Some(h(), t().take2(n - 1))
      case _                    =>  None
    }

  def take2Ans(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, _), i)  if i == 1   =>  Some(h(), (empty(), i))
      case (Cons(h, t), i)  if i > 0    =>  Some(h(), (t(), i - 1))
      case (Empty, _)                   =>  None
    }

  def takeWhile(predicate: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if predicate(h())   =>
        lazy val head = h()
        lazy val tail = t()
        cons(head, tail.takeWhile(predicate))

      case _  =>  empty()
    }

  def takeWhile2(predicate: A => Boolean): Stream[A] = {
    foldRight(empty(): Stream[A]) { (head, tail) =>
      if (!predicate(head)) {
        empty()
      } else {
        cons(head, tail)
      }
    }
  }

  //With use of unfold
  def takeWhileUnfold(predicate: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t)   if predicate(h())   =>  Some(h(), t())
      case _                                =>  None
    }

  def zipWith[B](zipStream: Stream[B]): Stream[(A,B)] =
    unfold((this, zipStream)) {
      case (Empty, _)                     =>  None
      case (_, Empty)                     =>  None
      case (Cons(h1, t1), Cons(h2, t2))   =>  Some((h1(), h2()), (t1(), t2()))
    }

  def zipWithAns[B](zipStream: Stream[B]): Stream[(A,B)] =
    unfold((this, zipStream)) {
      case (Cons(h1, t1), Cons(h2, t2))   =>  Some((h1(), h2()), (t1(), t2()))
      case _                              =>  None
    }

  def zipAll[B](zipStream: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, zipStream)) {
      case (Empty, Empty)                 =>  None
      case (Cons(h1, t1), Cons(h2, t2))   =>  Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty)          =>  Some((Some(h1()), None), (t1(), empty()))
      case (Empty, Cons(h2, t2))          =>  Some((None, Some(h2())), (empty(), t2()))
    }

  def drop(n: Int): Stream[A] =
    if (n <= 0) {
      this
    } else {
      this match {
        case Cons(_, t) =>
          lazy val tail = t()
          tail.drop(n - 1)

        case Empty  =>  empty()
      }
    }

  def append[B >: A](stream: Stream[B]): Stream[B] =
    this match {
      case Cons(head, tail)   =>  cons(head(), tail().append(stream))
      case Empty              =>  stream
    }

  def append2[B >: A](stream: => Stream[B]): Stream[B] =
    foldRight(stream) { (head, tail) =>
      cons(head, tail)
    }

  def map[B](mapping: A => B): Stream[B] =
    foldRight(empty(): Stream[B]) { (head, tail) =>
      cons(mapping(head), tail)
    }

  //With use of unfold
  def map2[B](mapping: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t)   =>  Some(mapping(h()), t())
      case Empty        =>  None
    }

  def flatMap[B](mapping: A => Stream[B]): Stream[B] =
    foldRight(empty(): Stream[B]) { (head, tail) =>
      mapping(head).append2(tail)
    }

  def filter(predicate: A => Boolean): Stream[A] =
    foldRight(empty(): Stream[A]) { (head, tail) =>
      if (predicate(head)) {
        cons(head, tail)
      } else {
        tail
      }
    }

  def find(predicate: A => Boolean): Option[A] =
    filter(predicate).headOption2

  def exists(predicate: A => Boolean): Boolean =
    this match {
      case Cons(h, t) =>  predicate(h())  ||  t().exists(predicate)
      case _          =>  false
    }

  def exists2(predicate: A => Boolean): Boolean =
    foldRight(false) { (a, b) =>
      predicate(a) || b
    }

  def forAll(predicate: A => Boolean): Boolean =
    this match {
      case Cons(h, _) if !predicate(h())  =>  false
      case Cons(_, t)                     =>  t().forAll(predicate)
      case Empty                          =>  true
    }

  def forAll2(predicate: A => Boolean): Boolean =
    foldRight(true) { (a,b) =>
      predicate(a) && b
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t)   =>  f(h(), t().foldRight(z)(f))
      case _            =>  z
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {

  def apply[A](elements: A*): Stream[A] =
    if (elements.isEmpty) {
      empty()
    } else {
      cons(elements.head, apply(elements.tail: _*))
    }

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A](): Stream[A] =
    Empty

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  //With use of unfold
  def constant2[A](a: A): Stream[A] =
    unfold(a) { a =>
      Some(a, a)
    }

  def constantAns[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  //With use of unfold
  def from2(n: Int): Stream[Int] =
    unfold(n) { a =>
      Some(a, a + 1)
    }

  def fibs(): Stream[Int] = {
    def next(prevPrev: Int, prev: Int): Stream[Int] = {
      val current = prevPrev + prev
      cons(current, next(prev, current))
    }

    val init = Stream(0, 1)
    val rest = next(0, 1)
    init.append2(rest)
  }

  def fibsAns(): Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  //With use of unfold
  def fibs2(): Stream[Int] =
    unfold(Stream(0, 1)) {
      case Cons(h, t)   =>
        lazy val f0 = h()
        lazy val tail = t()
        lazy val f1 = tail.headOption.get
        Some(f0, cons(f1, tail))
    }

  def fibs2Ans(): Stream[Int] =
    unfold((0,1)) {
      case (f0, f1) =>  Some(f0, (f1, f0 + f1))
    }

  def unfold[A,S](init: S)(f: S => Option[(A,S)]): Stream[A] =
    f(init) match {
      case Some((a,b))  =>  cons(a, unfold(b)(f))
      case None         =>  empty()
    }
}