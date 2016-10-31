package fpinscala.propertybasedtesting

import fpinscala.functionalstate.{SimpleRNG, RNG}
import fpinscala.nonstrictness.Stream
import Prop._


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (m, n, rng) => this.run(m, n, rng) match {
      case Passed => p.run(m, n, rng)
      case falsified => falsified
    }
  }

  def ||(p: Prop): Prop = Prop {
    (m, n, rng) => this.run(m, n, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(m, n, rng)
      case passed => passed
    }
  }

  def tag(msg: String): Prop = Prop {
    (m, n, rng) => run(m, n, rng) match {
      case Falsified(e, s) => Falsified(msg + "\n" + e, s)
      case Passed => Passed
    }
  }
}

object Prop {
  type MaxSize = Int
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (m, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }
}

object Test extends App {
  val smallInt = Gen.choose(-10, 10)
  val maxProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  Prop.run(maxProp)

  val sortedProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
    val list = ns.sorted
    list.forall(i => list.drop(list.lastIndexOf(i)).forall(k => k <= i))
    false
  }

  Prop.run(sortedProp)
}

