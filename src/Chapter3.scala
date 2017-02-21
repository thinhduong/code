package Chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](newHead: A, lst: List[A]) = Cons(newHead, tail(lst))

  def drop[A](lst: List[A], n: Int) = {
    def go(i: Int, lst: List[A]): List[A] =
      if (i == n) lst
      else go(i + 1, tail(lst))

    go(0, lst)
  }

  def dropWhile[A](lst: List[A], f : A => Boolean) = {
    def go(lst: List[A]): List[A] = lst match {
      case Nil => Nil
      case Cons(x, xs) => {
        if (f(x))
          go(xs)
        else
          xs
      }
    }

    go(lst)
  }

  def foldRight[A, B](ls: List[A], z:B)(f: (A, B) => B): B = ls match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](ls: List[A], z: B)(f: (B, A) => B): B = ls match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  //def foldLeftViaFoldRight[A, B](ls: List[A], z: B)(f: (B, A) => B): B =

  def length[A](ls: List[A]): Int = foldRight(ls, 0)((_, y) => y + 1)

  def lengthLeft[A](ls: List[A]): Int = foldLeft(ls, 0)((x, _) => x + 1)

  def sumLeft(ls: List[Int]): Int = foldLeft(ls, 0)(_+_)

  def productLeft(ls: List[Int]): Int = foldLeft(ls, 1)(_*_)

  def reverse[A](ls: List[A]): List[A] = foldLeft(ls, Nil: List[A])((x, y) => Cons(y, x))

  def init[A](lst: List[A]) = {
    def go(ls: List[A]): List[A] = ls match {
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, go(xs))
    }

    go(lst)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}


