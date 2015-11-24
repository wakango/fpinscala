package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def toList: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => {
        go(t(), h() :: acc)
      }
    }
    go(this, Nil).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, t) if n == 1 => cons(h(), empty)
      case _ => Empty
    }
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n >= 1 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = sys.error("todo")

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def map[B](f: A => B): Stream[B] = {
    unfold(this){
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)){
      case (Cons(h, t), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), i) if i >= 1 => Some((h(), (t(), i - 1)))
      case _ => None
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

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

  def constant[A](a: A): Stream[A] = {
    lazy val c: Stream[A] = Cons(() => a, () => c)
    c
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def fibs: Stream[Int] = {
    def go(cur: Int, next: Int): Stream[Int] = {
      cons(cur, go(next, cur + next))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

  def fibsViaUnfold: Stream[Int] = {
    unfold[Int, (Int, Int)]((0, 1)){ case (a, b) => Some((a, (b, a + b))) }
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(s => Some((s, s + 1)))
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(_ => Some(a, a))
  }
}