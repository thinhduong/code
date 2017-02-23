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

  def map2[B](f: A => B): Stream[B] =
    Stream.unfold(this)(xs => xs match {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    })

  def take2(n: Int): Stream[A] =
    Stream.unfold((0, this))(x => {
      if (x._1 == n)
        None
      else
        x._2 match {
          case Empty => None
          case Cons(h, t) => Some((h(), (x._1 + 1, t())))
        }
    })

  def takeWhile3(f: A => Boolean): Stream[A] =
    Stream.unfold(this)(x =>  x match {
      case Cons(h, t) =>
        if (f(h())) Some((h(), t()))
        else None
      case _ => None
    })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2))(xs => xs match {
      case (Empty, Empty) => None
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    })

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

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  def fibs: Stream[Long] = {
    def go(a: Long, b: Long): Stream[Long] =
      Stream.cons(a + b, go(b, a + b))
    Stream.cons(1, Stream.cons(1, go(1, 1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => Empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }

  def constant2[A](a: A): Stream[A] =
    unfold(a)(x => Some(x, x))

  def from2(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, x+1))

  def fibs2: Stream[Long] =
    unfold((1, 1))(x => Some(x._1, (x._2, x._1 + x._2)))
}
