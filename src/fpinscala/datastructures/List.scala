package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def head[A](l: List[A]): Option[A] = l match {
    case Nil => None
    case Cons(a, _) => Some(a)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](a: A, l: List[A]): List[A] =
    Cons(a, l)

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }

  @annotation.tailrec
  def dropWhile[A](l: List[A])(p: A => Boolean): List[A] = l match {
    case Cons(x, xs) if p(x) => dropWhile(xs)(p)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def naiveFoldRight[E, ACC](as: List[E], z: ACC)(f: (E, ACC) => ACC): ACC =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, naiveFoldRight(xs, z)(f))
    }

  /*
  * The function used as accumulator of foldLeft allows to stack values in reverse order and evaluate only when the stack
  * is completely formed: foldLeft returns a function which is to be called on the original accumulator z
  *
  * */
  def foldRight[E, ACC](as: List[E], z: ACC)(f: (E, ACC) => ACC): ACC =
    foldLeft(as, (x: ACC) => x)((id, a) => b => id(f(a, b)))(z)

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](l: List[A]) =
    foldRight(l, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[E, ACC](as: List[E], z: ACC)(f: (ACC, E) => ACC): ACC =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumLeft(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def productLeft(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def lengthLeft[A](l: List[A]) =
    foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, e) => Cons(e, acc))

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  def add1(l: List[Int]): List[Int] =
  //foldRight(l, Nil: List[Int])((e, acc) => Cons(e + 1, acc))
    map(l)(1.+)

  def asString(ds: List[Double]): List[String] =
  //foldRight(ds, Nil: List[String])((e, acc) => Cons(e.toString, acc))
    map(ds)(_.toString)

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((e, acc) => Cons(f(e), acc))

  def filter1[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((e, acc) => if (f(e)) Cons(e, acc) else acc)

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def combine(l: List[Int], m: List[Int]): List[Int] = (l, m) match {
    case (Cons(i, is), Cons(j, js)) => Cons(i + j, combine(is, js))
    case _ => Nil
  }

  def zipWith[A, B, C](l: List[A], m: List[B])(f: (A, B) => C): List[C] = (l, m) match {
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
    case _ => Nil
  }

  def startsWith[A](l: List[A])(prefix: List[A]): Boolean =
    foldLeft(zipWith(l, prefix)((r, c) => r == c), false)(_ && _)

  def hasSubsequence[A](sup: List[A])(sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(r, rs), Cons(c, cs)) if r == c => startsWith(rs)(cs)
    case (Cons(r, rs), Cons(c, cs)) => startsWith(rs)(sub)
    case (Nil, _) => false
    case (_, Nil) => false
  }

  /*
  * Correction proposee:
  *
    @annotation.tailrec
    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
      case (_,Nil) => true
      case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
      case _ => false
    }
    @annotation.tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_,t) => hasSubsequence(t, sub)
    }
  * */

}
