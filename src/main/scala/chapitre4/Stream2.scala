package chapitre4

import scala.annotation.tailrec

trait Stream2[+A] {
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def take(n: Int): Stream2[A] = this match {
    case Cons(hd, tl) if n > 1 => Stream2.cons(hd(), tl().take(n-1))
    case Cons(hd, tl) if n == 1 => Stream2.cons(hd(), Stream2.empty)
    case Empty => Stream2.empty
  }


  def takeWhile(p: A => Boolean): Stream2[A] = this match {
    case Cons(hd, tl) if p(hd())  => Stream2.cons(hd(), tl().takeWhile(p))
    case _ => Stream2.empty
  }

  def takeWhile2(p: A => Boolean): Stream2[A] =
    foldRight(Stream2.empty[A])((a, b) => if (p(a)) Stream2.cons(a, b) else Empty)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream2[B] =
    foldRight(Stream2.empty[B])((a, b) => Stream2.cons(f(a), b))

  def filter(p: A => Boolean): Stream2[A] =
    foldRight(Stream2.empty[A])((a, b) => if (p(a)) Stream2.cons(a, b) else b)

  def append[B>:A](stream: Stream2[B]): Stream2[B] =
    foldRight(stream)((a, b) => Stream2.cons(a, b))

  def flatMap[B](f: A => Stream2[B]): Stream2[B] =
    foldRight(Stream2.empty[B])((a, b) => f(a).append(b))

  def mapUsingUnfold[B](f: A => B): Stream2[B] =
    Stream2.unfold (this) {
      case Cons(hd, tl) => Some((f(hd()), tl()))
      case _ => None
    }

  def takeUsingUnfold(n: Int): Stream2[A] =
    Stream2.unfold ((this, n)) {
      case (Cons(hd, tl), p) if p > 1 => Some((hd(), (tl(), p - 1)))
      case (Cons(hd, tl), p) if p == 1 => Some((hd(), (Empty, p - 1)))
      case _ => None
    }

  def takeWhileUsingUnfold(p: A => Boolean): Stream2[A] =
    Stream2.unfold (this) {
      case Cons(hd, tl) if p(hd()) => Some((hd(), tl()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream2[B])(f: (A,B) => C): Stream2[C] =
    Stream2.unfold ((this, s2)) {
      case (Cons(h, t), Cons(hd, tl)) => Some(f(h(), hd()), (t(), tl()))
      case _ => None
    }

  def zipAll[B](s2: Stream2[B]): Stream2[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream2[B])(f: (Option[A], Option[B]) => C): Stream2[C] =
    Stream2.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), None) -> (t(), Empty))
      case (Empty, Cons(h, t)) => Some(f(None, Some(h())) -> (Empty, t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))
    }

  def startsWith[A](s: Stream2[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h,h2) => h == h2
    }

  def zip[B](s2: Stream2[B]): Stream2[(A,B)] =
    zipWith(s2)((_,_))

  def tails: Stream2[Stream2[A]] =
    Stream2.unfold (this) {
      case Cons(hd, tl) => Some(Stream2.cons(hd(), tl()) -> tl())
      case _ => None
    }

  def scanRight[B](z: B)(f: (A,A) => B): Stream2[B] = ???

  override def toString = this match {
    case Cons(hd, tl) => "Cons(" + hd() + ", ?)"
    case _ => "Empty"
  }
}
case object Empty extends Stream2[Nothing]
case class Cons[+A](h: () => A, t: () => Stream2[A]) extends Stream2[A]

object Stream2 {
  def cons[A](hd: => A, tl: => Stream2[A]): Stream2[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream2[A] = Empty

  def apply[A](as: A*): Stream2[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream2[A] = cons(a, constant(a))

  def from(n: Int): Stream2[Int] = cons(n, from(n+1))

  def fibs(n0: Int, n1: Int): Stream2[Int] =
    cons(n1, fibs(n1, n0 + n1))

  val onesUsingUnfold: Stream2[Int] =
    unfold (1) (s2 => Some((1, s2)))
  //((a: Int, s: Int) => unfold (1) (s2 => Some((1, s2)))) (1, 1)

  def fromUsingUnfold(n: Int): Stream2[Int] =
    unfold (n) (s2 => Some((s2, s2 + 1))) // notice we give a step by incrementing the generator with a given value

  def constantUsingUnfold[A](a: A): Stream2[A] =
    unfold (a) (s2 => Some((s2, s2)))

  def fibsUsingUnfold(n0: Int, n1: Int): Stream2[Int] =
    unfold ((n0, n1)) { case (v0, v1) => Some((v0, (v1, v0 + v1))) }

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails exists (_.startsWith(s2))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream2[A] = {
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case _ => empty
    }
  }
}
