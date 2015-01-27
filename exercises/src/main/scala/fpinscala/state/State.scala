package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i == Int.MinValue)
      nonNegativeInt(r)
    else
      (math.abs(i), r)
  }

  def double(rng: RNG): (Double, RNG) =  {
    val (i, r) = nonNegativeInt(rng)
    if (i == Int.MaxValue)
      double(r)
    else
      (i.toDouble/Int.MaxValue.toDouble, r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val (d, res) = double(r)
    ((i, d), res)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (pair, r) = intDouble(rng)
    (pair.swap, r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def doubleUsingMap: Rand[Double] =
    map(nonNegativeInt)(_.toDouble/(Int.MaxValue + 1))

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count < 1)
      (Nil, rng)
    else {
      val (l, r) = ints(count - 1)(rng)
      val (i, res) = nonNegativeInt(r)
      (i :: l, res)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs match {
    case Nil => (List[A](), rng)
    case h :: t => {
      val (l, r1) = sequence(t)(rng)
      val (a, r2) = h(r1)
      (a :: l, r2)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =  rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    val maxVal = Int.MaxValue - Int.MaxValue%n
    flatMap(nonNegativeInt)(a => if (a < maxVal) unit(a % n) else nonNegativeLessThan(n))
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def mapUsingFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => (rng => (f(a), rng)))

  def map2UsingFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a =>
      flatMap(rb)(b =>
        r => (f(a, b), r)
      )
    )
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2UsingFlatMap(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)
}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    State(
      s => {
        val (a, ss) = run(s)
        (f(a), ss)
      }
    )

  def map2[B, C](other: State[S, B])(f: (A, B) => C): State[S, C] =
    State(
      s => {
        val (a, s1) = run(s)
        val (b, s2) = other.run(s1)
        (f(a, b), s2)
      }
    )

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a, ss) = run(s)
        f(a).run(ss)
      }
    )
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def unit[S,A](a: => A): State[S,A] = State(s => (a, s))
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
