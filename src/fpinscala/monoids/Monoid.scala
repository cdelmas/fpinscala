package fpinscala.monoids

import fpinscala.datastructures.{Tree, Leaf, Branch}

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

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val monoid = new Monoid[Option[(Int, Int, Boolean)]] {
      override def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        (o1, o2) match {
          case (Some((min1, max1, p)), Some((min2, max2, q))) =>
            Some((min1 min min2, max1 max max2, p && q && max1 <= min2))
          case (x, None) => x
          case (None, x) => x
        }

      override val zero: Option[(Int, Int, Boolean)] = None
    }

    foldMapV(ints, monoid)(i => Some((i, i, true))) forall {
      _._3
    }
  }
}

sealed trait WC

case class Stub(chars: String) extends WC

case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
  val wcMonoid: Monoid[WC] = new Monoid[WC] {

    override def op(c1: WC, c2: WC): WC = (c1, c2) match {
      case (Stub(ls), Stub(rs)) => Stub(ls + rs)
      case (Stub(ls), Part(l, w, r)) => Part(ls + l, w, r)
      case (Part(l, w, r), Stub(rs)) => Part(l, w, r + rs)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }

    override val zero: WC = Stub("")
  }

  def count(text: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    def unstub(s: String): Int = s.length min 1

    Monoid.foldMapV(text.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }
}

trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((acc, a) => mb.op(acc, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List[A]())(_ :: _)
}

object Folds {

  val listFold: Foldable[List] = new Foldable[List] {

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def toList[A](fa: List[A]): List[A] = fa
  }

  val indexedSeqFold: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def toList[A](fa: IndexedSeq[A]): List[A] = fa.toList
  }

  val streamFold: Foldable[Stream] = new Foldable[Stream] {

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def toList[A](fa: Stream[A]): List[A] = fa.toList
  }

  val treeFold: Foldable[Tree] = new Foldable[Tree] {

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(v) => f(z, v)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
  }

  val optionFold: Foldable[Option] = new Foldable[Option] {

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def toList[A](fa: Option[A]): List[A] = fa.toList
  }

}

object Compose {

  import Monoid._

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {

    override def op(p1: (A, B), p2: (A, B)): (A, B) = (a.op(p1._1, p2._1), b.op(p1._2, p2._2))

    override def zero: (A, B) = (a.zero, b.zero)
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {

    override def op(a1: (A) => B, a2: (A) => B): (A) => B = a => B.op(a1(a), a2(a))

    override def zero: (A) => B = a => B.zero
  }

}
