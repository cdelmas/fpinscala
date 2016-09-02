package fpinscala.parallelism

import java.util.concurrent._


object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def async[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)


  def map2_basic[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  // respect the timeout
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)

      // requires an implementation of Future that register the computation time
      new Future[C] {
        override def isCancelled: Boolean = false

        override def get(): C = f(af.get, bf.get)

        override def get(timeout: Long, timeUnit: TimeUnit): C = {
          val start = System.currentTimeMillis()
          val a = af.get(timeout, timeUnit)
          val elapsed = System.currentTimeMillis() - start
          val remaining = timeUnit.toMillis(timeout) - elapsed
          val b = bf.get(remaining, TimeUnit.MILLISECONDS)
          f(a, b)
        }

        override def cancel(b: Boolean): Boolean = false

        override def isDone: Boolean = true
      }
    }

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => async(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case h :: t => map2(h, sequence(t))(_ :: _)
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    sequence(ps.map(asyncF(f)))
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val seq = as map asyncF(a => if (f(a)) List(a) else List())
    map(sequence(seq))(_.flatten)
  }

  def reduce[A](as: IndexedSeq[A], zero: A)(f: (A, A) => A): Par[A] = {
    if (as.isEmpty) {
      unit(zero)
    } else {
      val (h, t) = as.splitAt(as.length / 2)
      map2(fork(reduce(h, zero)(f)), fork(reduce(t, zero)(f)))(f)
    }
  }

  def map3[A, B, C, D](fa: Par[A], fb: Par[B], fc: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(map2(fa, fb)((a, b) => (c: C) => f(a, b, c)), fc)(_ (_))

  def map4[A, B, C, D, E](fa: Par[A], fb: Par[B], fc: Par[C], fd: Par[D])(f: (A, B, C, D) => E): Par[E] =
    map2(map2(map2(fa, fb)((a, b) => (c: C) => (d: D) => f(a, b, c, d)), fc)(_ (_)), fd)(_ (_))

  def map5[A, B, C, D, E, F](fa: Par[A], fb: Par[B], fc: Par[C], fd: Par[D], fe: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
    map2(map2(map2(map2(fa, fb)((a, b) => (c: C) => (d: D) => (e: E) => f(a, b, c, d, e)), fc)(_ (_)), fd)(_ (_)), fe)(_ (_))

  def choice_basic[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)

  def choiceN_basic[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es =>
      run(es)(choices(run(es)(n).get))

  def choice_with_choiceN_basic[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN_basic(map(cond) { if (_) 0 else 1 })(List(t, f))

  def choiceMap_basic[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => choices(run(es)(key).get)(es)

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => choices(run(es)(pa).get)(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond) { if (_) t else f }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choices(_))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    chooser(key)(choices)

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(a).get()(es)

  def flatMap[A, B](a: Par[A])(f: A => Par[B]) =
    join(map(a)(f))

  def join_with_flatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)

  def map2_with_flatMap_and_unit[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = {
    flatMap(pa)(a => flatMap(pb)(b => unit(f(a, b))))
  }

}

