trait RNG {
  type Rand[+A] = RNG => (A, RNG)

  def nextInt: (Int, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, nextRNG) => (0, nextRNG)
    case (nextInt, nextRNG) =>
      if (nextInt >= 0) (nextInt, nextRNG)
      else (-nextInt, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nonNegInt, nextRNG) = nonNegativeInt(rng)

    (nonNegInt.toDouble / Int.MaxValue, nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nextInt, nextRNG) = rng.nextInt

    val (nextDouble, nextRNG2) = double(nextRNG)

    ((nextInt, nextDouble), nextRNG2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((nextInt, nextDouble), nextRNG) = intDouble(rng)

    ((nextDouble, nextInt), nextRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (nextDouble1, nextRNG1) = double(rng)
    val (nextDouble2, nextRNG2) = double(nextRNG1)
    val (nextDouble3, nextRNG3) = double(nextRNG2)

    ((nextDouble1, nextDouble2, nextDouble3), nextRNG3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(i: Int, ret: (List[Int], RNG)): (List[Int], RNG) = {
      if (i >= count)
        ret
      else {
        val (nextInt, nextRNG) = rng.nextInt
        go(i + 1, (nextInt :: ret._1, nextRNG))
      }
    }

    go(0, (Nil, rng))
  }

  def double2: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)

    (f(a, b), rng2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    def go(xs: List[Rand[A]], ret: List[A]): Rand[List[A]] = rng => xs match {
      case Nil => unit(ret)(rng)
      case h :: t => {
        val (a, nextRng) = h(rng)
        go(t, a :: ret)(nextRng)
      }
    }

    go(fs, Nil: List[A])(rng)
  }

  def ints2(count: Int)(rng: RNG): Rand[List[Double]] = {
    val randList = (1 to count).map(_ => double2).toList
    sequence(randList)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(i => unit(f(i)))

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => {
    flatMap(rb)(b => unit(f(a, b)))
  })

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      unit(mod)
    else
      nonNegativeLessThan(n)
  })

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(i => i + 1)
}

case class State[S, +A](run: S => (A, S)) {
  def unit[B](a: B): State[S, B] = State(s => (a, s))

  def map[B >: A, C](s: State[S, B])(f: B => C): State[S, C] = State(s1 => {
    val (a, s2) = s.run(s1)
    (f(a), s2)
  })

  def map2[B >: A, C, D](sb: State[S, B], sc: State[S, C])(f: (B, C) => D): State[S, D] = State(s1 => {
    val (b, s2) = sb.run(s1)
    val (c, s3) = sc.run(s2)

    (f(b,c), s3)
  })

  def flatMap[B >: A, C](sb: State[S, B])(f: B => State[S, C]): State[S, C] = State(s1 => {
    val (b, s2) = sb.run(s1)
    f(b).run(s2)
  })

  def sequence[B](fs: List[State[S, B]]): State[S, List[B]] = State(s1 => {

    def go(xs:  List[State[S, B]], ret: List[B]): State[S, List[B]] = State(s1 => xs match {
      case Nil => unit[List[B]](ret).run(s1)
      case h :: t => {
        val (b, s2) = h.run(s1)
        go(t, b :: ret).run(s2)
      }
    })

    go(fs, Nil: List[B]).run(s1)
  })
}

object State {

}

sealed trait Input

case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)


