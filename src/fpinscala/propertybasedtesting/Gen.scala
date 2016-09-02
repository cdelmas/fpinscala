package fpinscala.propertybasedtesting

import fpinscala.functionalstate.{RNG, State}

/**
  * Created by c.delmas on 22/07/2016.
  */
case class Gen[A](sample: State[RNG, A])

object Gen {

  def choose(start:Int, stopExclusive:Int): Gen[Int] =
    Gen(State(RNG.map(RNG.positiveLessThan(stopExclusive - start))(_ + start)))

  def unit[A](a: => A): Gen[A] =
    Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  
}



