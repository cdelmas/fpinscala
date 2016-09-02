package fpinscala.functionalstate

sealed trait RNG {

  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed << 16).toInt
    (n, nextRNG)
  }

}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match {
      case (i, r) => (i % 2 == 0, r)
    }

  def positiveLessThan(upperBound: Int)(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (i, r) => (i % upperBound, r)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def nextDouble(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r) = nextDouble(r1)
    ((i, d), r)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = nextDouble(rng)
    val (d2, r2) = nextDouble(r1)
    val (d3, r) = nextDouble(r2)
    ((d1, d2, d3), r)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(n: Int)(r0: RNG)(l: List[Int]): (List[Int], RNG) = {
      if (n == 0) (l, r0)
      else {
        val (i, r) = r0.nextInt
        go(n - 1)(r)(i :: l)
      }
    }
    go(count)(rng)(List())
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double(rng: RNG): Rand[Double] =
    map(int)(i => i / (Int.MaxValue.toDouble + 1))

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r0) = ra(rng)
      val (b, r1) = rb(r0)
      (f(a, b), r1)
    }

  // In `sequence`, the base case of the fold is a `unit` action that returns
  // the empty list. At each step in the fold, we accumulate in `acc`
  // and `f` is the current element in the list.
  // `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
  // We map over that to prepend (cons) the element onto the accumulated list.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)((a) => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)((a) => map(rb)((b) => f(a, b)))

}
