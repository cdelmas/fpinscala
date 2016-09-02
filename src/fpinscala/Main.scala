package fpinscala

/**
 * (c) Lectra.
 * @author c.delmas
 */
object Main {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatResult(name: String, x: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, x, f(x))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, acc * n)
    go(n, 1)
  }

  def fibo(n: Int): Int = {
    @annotation.tailrec
    def aux(n: Int, prev: Int, acc: Int): Int = {
      if (n == 0) acc
      else aux(n - 1, acc, acc + prev)
    }
    aux(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 6, factorial))
    println(formatResult("fibonacci number at position", 7, fibo))
    println(findFirst(Array(2, 4, 6, 7), (e: Int) => e > 6) getOrElse -1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Option[Int] = {
    @annotation.tailrec
    def loop(n: Int): Option[Int] =
      if (n >= as.length) None
      else if (p(as(n))) Some(n)
      else loop(n + 1)
    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(index: Int): Boolean =
      if (index >= as.length) true
      else if (!ordered(as(index - 1), as(index))) false
      else loop(index + 1)
    loop(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

}
