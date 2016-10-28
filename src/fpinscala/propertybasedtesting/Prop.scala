package fpinscala.propertybasedtesting

import fpinscala.functionalstate.RNG
import fpinscala.nonstrictness.Stream
import Prop._


case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (n, rng) => this.run(n, rng) match {
      case Passed => p.run(n, rng)
      case falsified => falsified
    }
  }

  def ||(p: Prop): Prop = Prop {
    (n, rng) => this.run(n, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(n, rng)
      case passed => passed
    }
  }

  def tag(msg: String): Prop = Prop {
    (n, rng) => run(n, rng) match {
      case Falsified(e, s) => Falsified(msg + "\n" + e, s)
      case Passed => Passed
    }
  }
}

object Prop {
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

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}
