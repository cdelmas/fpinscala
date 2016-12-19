package fpinscala.monoids


trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

// instance example
object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(s1: String, s2: String): String = s1 + s2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(l1: List[A], l2: List[A]): List[A] = l1 ++ l2

    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {

    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {

    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {

    override def op(a1: A => A, a2: A => A): A => A = a1 andThen a2

    override def zero: A => A = identity
  }

  def dual[A](m: Monoid[A => A]): Monoid[A => A] = new Monoid[A => A] {

    override def op(a1: A => A, a2: A => A): A => A = m.op(a2, a1)

    override def zero: A => A = m.zero
  }

  import fpinscala.propertybasedtesting._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen)(a =>
      m.op(a, m.zero) == a
        && m.op(m.zero, a) == a) &&
      forAll(for {
        x <- gen
        y <- gen
        z <- gen
      } yield (x, y, z))(t => m.op(m.op(t._1, t._2), t._3) == m.op(t._1, m.op(t._2, t._3)))


  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.isEmpty) m.zero
    else if (v.length == 1) f(v(0))
    else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  import fpinscala.parallelism.Par._

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {

    override def op(a1: Par[A], a2: Par[A]): Par[A] = map2(a1, a2)(m.op)

    override def zero: Par[A] = unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    flatMap(parMap(v)(f)) {
      foldMapV(_, par(m))(a => async(f(a)))
    }
  }
}

