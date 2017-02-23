package Chapter5

sealed trait Stream[+A]{
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def reverse: Stream[A] = {
    def go(xs: Stream[A], ret: Stream[A]): Stream[A] = xs match {
      case Empty => ret
      case Cons(h, t) => go(t(), Stream.cons(h(), ret))
    }

    go(this, Empty)
  }

  def take(n: Int): Stream[A] = {
    def go(i: Int, s: Stream[A], ret: Stream[A]): Stream[A] = {
      if (i == n) ret.reverse
      else s match {
        case Empty => ret.reverse
        case Cons(h, t) => go(i + 1, t(), Stream.cons(h(), ret))
      }
    }

    go(0, this, Empty)
  }

  def drop(n: Int): Stream[A] = {
    def go(i: Int, s: Stream[A]): Stream[A] =
      if (i == n) s
      else s match {
        case Empty => Empty
        case Cons(_, t) => go(i+1, t())
      }

    go(0, this)
  }

  def takeWhile(f: A => Boolean): Stream[A] = {
    def go(s: Stream[A], ret: Stream[A]): Stream[A] = s match {
      case Empty => ret.reverse
      case Cons(h, t) =>
        if (f(h()))
          go(t(), Stream.cons(h(), ret))
        else
          ret.reverse
    }

    go(this, Empty)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((x, y) => p(x) && y)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((x, y) => {
      if (p(x)) Stream.cons(x, y)
      else y
    })

  def headOption2: Option[A] =
    foldRight(None: Option[A])((x, _) => {
      try {
        Some(x)
      }
      catch { case _: Throwable => None}
    })

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((x, y) => Stream.cons(f(x), y))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((x, y) => {
      if (f(x))
        Stream.cons(x, y)
      else
        y
    })

  def append[B >: A](a: => B): Stream[B] =
    foldRight(Stream(a))((x, y) => Stream.cons(x, y))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((x, y) => {
      def go(xs: Stream[B], ret: Stream[B]): Stream[B] = xs match {
        case Empty => ret
        case Cons(h, t) => go(t(), Stream.cons(h(), ret))
      }

      go(f(x).reverse, y)
    })
}

case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val hd = head
    lazy val tl = tail

    Cons(() => hd, () => tl)
  }

  def empty[A] = Empty

  def apply[A](xs: A*): Stream[A] =
    if (xs.isEmpty) empty
    else cons(xs.head, apply(xs.tail: _*))
}

object Chapter5 {


}
