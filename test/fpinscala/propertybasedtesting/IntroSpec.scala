package fpinscala.propertybasedtesting

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop._

class IntroSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  val intList = Gen.listOf(Gen.choose(0, 100))

  property("reversing a reversed list gives the same as original") {
    forAll(intList) { ns =>
      ns.reverse.reverse should equal(ns)
    }
  }

  property("tail of the reversed list is the head of the original") {
    forAll(intList) { ns =>
      ns.headOption should equal(ns.reverse.lastOption)
    }
  }

}

