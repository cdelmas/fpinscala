package fpinscala.monads

import fpinscala.parallelism.Par
import fpinscala.parallelism.Par._
import fpinscala.functionalstate._

trait Monad[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(List[A]()))((e, acc) => map2(e, acc)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = la.foldRight(unit(List[B]()))((e, acc) => map2(f(e), acc)(_ :: _))

  // replicate simply give the same value n times, so here it will repeat the monadic value in a list
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]()))((a, as) =>
      compose(f, (b: Boolean) => if (b) map2(unit(a), as)(_ :: _) else as)(a))

  def flatMap_[A, B](ma: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => ma, f)()

}

object Monads {

  val parMonad = new Monad[Par] {

    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = unit(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {

    override def unit[A](a: => A): Stream[A] = unit(a)

    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f
  }

  // Since `State` is a binary type constructor, we need to partially apply it
  // with the `S` type argument. Thus, it is not just one monad, but an entire
  // family of monads, one for each type `S`. One solution is to create a class
  // `StateMonads` that accepts the `S` type argument and then has a _type member_
  // for the fully applied `State[S, A]` type inside:
  class StateMonads[S] {
    type StateS[A] = State[S, A]

    val monad = new Monad[StateS] {
      override def unit[A](a: => A): StateS[A] = unit(a)

      override def flatMap[A, B](ma: StateS[A])(f: (A) => StateS[B]): StateS[B] = ma flatMap f
    }
  }

  // OR We can create an anonymous class inline, inside parentheses, and project out
  // its type member, `lambda`:
  def stateMonad[S] = new Monad[({type lambda[X] = State[S, X]})#lambda] {
    override def unit[A](a: => A): State[S, A] = unit(a)

    override def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma flatMap f
  }

}