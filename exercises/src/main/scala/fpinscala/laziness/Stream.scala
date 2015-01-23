package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    uncons match {
      case Some(c) =>
        if (n > 0)
          Stream.cons(c.h(), c.t().take(n - 1))
        else
          Stream.empty

      case _ => Stream.empty
    }

  def drop(n: Int): Stream[A] = sys.error("todo")

  def takeWhile(p: A => Boolean): Stream[A] = sys.error("todo")

  def forAll(p: A => Boolean): Boolean = sys.error("todo")

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def uncons: Option[Cons[A]]

  def toList: List[A] = uncons match {
    case None => Nil
    case Some(c) => c.h() :: c.t().toList
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream[B]())((a, s) => Stream.cons(f(a), s))

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, that)) {
      case (ss1, ss2) =>
        ss1.uncons.flatMap(c1 =>
          ss2.uncons.map(c2 =>
            (f(c1.h(), c2.h()), (c1.t(), c2.t()))))
    }

  def zip[B, C](that: Stream[B]): Stream[(A, B)] = zipWith(that)((a, b) => (a, b))

}

case object Empty extends Stream[Nothing] {
  override val uncons = None
}
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  val uncons = Some(this)
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  def fromUsingUnfold(n: Int): Stream[Int] =
    unfold(n)(nn => Some((nn, nn + 1)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def go(zz: S, ss: Stream[A]): Stream[A] = f(zz) match {
      case Some((a, s)) => Stream.cons(a, go(s, ss))
      case _ => Stream.empty
    }
    go(z, Stream.empty)
  }
}