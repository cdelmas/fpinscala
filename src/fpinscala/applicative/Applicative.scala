package fpinscala.applicative

import fpinscala.monads.{Functor, Monad}

trait Applicative[F[_]] extends Functor[F] {

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_ (_))

  def unit[A](a: => A): F[A]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] = {
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }

  def map4[A, B, C, D, E](fa: F[A],
                          fb: F[B],
                          fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
}

object Monads {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {

    override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = ma match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    override def unit[A](a: => A): Either[E, A] = Right(a)
  }

}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicatives {

  def validationApplicative[E]: Applicative[({type l[X] = Validation[E, X]})#l] = new Applicative[({type l[X] = Validation[E, X]})#l] {

    override def map2[A, B, C](va: Validation[E, A], vb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (va, vb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Success(_), e@Failure(_, _)) => e
      case (e@Failure(_, _), Success(_)) => e
      case (Failure(e1, es1), Failure(e2, es2)) => Failure(e1, es1 ++ Vector(e2) ++ es2)
    }

    override def unit[A](a: => A) = Success(a)
  }

}