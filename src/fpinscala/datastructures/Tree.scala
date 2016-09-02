package fpinscala.datastructures

/**
 * (c) Lectra.
 * @author c.delmas
 */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size1[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def size[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximum1(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(l, r) => maximum1(l) max maximum1(r)
  }

  def maximum(t: Tree[Int]): Int =
    fold(t)(x => x)(_ max _)

  def depth1[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth1(l) max depth1(r))
  }

  def depth[A](t: Tree[A]): Int =
    fold(t)(_ => 0)(1 + _.max(_))

  def map1[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map1(l)(f), map1(r)(f))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def fold[A, B](t: Tree[A])(f: A => B)(c: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => c(fold(l)(f)(c), fold(r)(f)(c))
  }
}