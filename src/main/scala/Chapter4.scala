package Chapter4

trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) match {
    case Some(Some(x)) => Some(x)
    case _ => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(x) =>
      if (f(x)) Some(x)
      else None
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a match {
      case None => None
      case Some(x) => b match {
        case None => None
        case Some(y) => Some(f(x, y))
      }
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def go(xs: List[Option[A]], ret: List[A]): Option[List[A]] = xs match {
      case Nil => Some(ret.reverse)
      case h :: t => h match {
        case None => None
        case Some(x) => go(t, x :: ret)
      }
    }

    go(a, List())
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def go(xs: List[A], ret: List[B]): Option[List[B]] = xs match {
      case Nil => Some(ret.reverse)
      case h :: t => f(h) match {
        case None => None
        case Some(x) => go(t, x :: ret)
      }
    }

    go(a, List())
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Chapter4 {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    val ex = mean(xs)
    val ex2 = mean(xs.map(x => x * x))

    ex2.flatMap(x => ex.map(y => y * y) match {
      case None => None
      case Some(z) => Some(x - z)
    })
  }
}



