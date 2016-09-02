package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}


/**
 * (c) Lectra.
 * @author c.delmas
 */
class OptionSpec extends FlatSpec with Matchers {

  def mean(seq: Seq[Double]): Option[Double] = {
    if (seq.isEmpty) None
    else Some(seq.sum / seq.length)
  }

  def variance(seq: Seq[Double]): Option[Double] = {
    mean(seq).flatMap(m => mean(seq.map(x => math.pow(x - m, 2))))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(av), Some(bv)) => Some(f(av, bv))
    case _ => None
  }

  def map2_[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence[A](aos: List[Option[A]]): Option[List[A]] =
    aos.foldRight[Option[List[A]]](Some(Nil))((a, as) => map2(a, as)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((a, acc) => map2(f(a), acc)(_ :: _))

  "mean" should "be None on empty sequence" in {
    mean(List()) should be(None)
  }

  it should "be 3 for List(1,2,3,4,5)" in {
    mean(List(1.0, 2.0, 3.0, 4.0, 5.0)) should be(Some(3.0))
  }

  "variance" should "be None" in {
    variance(Nil) should be(None)
  }

  it should "be 29.33" in {
    variance(List(12, 14, 6.5, 20, 5, 17.5)).getOrElse(0.0) should ===(29.33 +- 0.01)
  }

  "sequence" should "give empty list for an empty list" in {
    sequence(Nil) should be(Some(Nil))
  }

  it should "give an Option of list containing the values" in {
    sequence(List(Some(1), Some(4), Some(2))) should be(Some(List(1, 4, 2)))
  }

  it should "give None if the list contains at least one None" in {
    sequence(List(None)) should be(None)
    sequence(List(Some(1), None)) should be(None)
    sequence(List(None, Some("toto"))) should be(None)
  }

  "traverse" should "give a Some(empty) on empty list" in {
    traverse(List())((a) => Some(a)) should be(Some(List()))
  }

  it should "give a Some(List)) with only Some as result of f" in {
    traverse(List(1, 2, 3, 4, 5))((i) => Some(i)) should be(Some(List(1, 2, 3, 4, 5)))
  }

  it should "give a None with one None as result of f" in {
    traverse(List(1, 2, 3, 4, 5))((i) => if (i == 4) None else Some(i)) should be(None)
  }
}
