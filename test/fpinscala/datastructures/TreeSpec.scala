package fpinscala.datastructures

import fpinscala.datastructures.Tree._
import org.scalatest.{FlatSpec, Matchers}

/**
 * (c) Lectra.
 * @author c.delmas
 */
class TreeSpec extends FlatSpec with Matchers {

  "size" should "be 1 for a Leaf" in {
    Tree.size(Leaf("toto")) should be(1)
  }

  it should "be 3 for a node with two leaves" in {
    Tree.size(Branch(Leaf("toto"), Leaf("tutu"))) should be(3)
  }

  "maximum" should "return the leaf value for a Tree = a Leaf" in {
    maximum(Leaf(5)) should be(5)
  }

  it should "find the greatest integer in 4-o-6 to be 6" in {
    maximum(Branch(Leaf(4), Leaf(6))) should be(6)
  }

  it should "find 10 to be the maximum of the tree" in {
    maximum(Branch(Branch(Branch(Leaf(4), Leaf(2)), Leaf(2)), Branch(Leaf(10), Leaf(10)))) should be(10)
  }

  "depth" should "be 0 for a Tree = a Leaf" in {
    depth(Leaf(7)) should be(0)
  }

  it should "be 1 for a one level tree" in {
    depth(Branch(Leaf(0), Leaf(0))) should be(1)
  }

  it should "be 2 for a balanced two levels tree" in {
    depth(Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(3), Leaf(4)))) should be(2)
  }

  it should "be 2 for an unbalanced two levels tree" in {
    depth(Branch(Leaf(0), Branch(Leaf(3), Leaf(4)))) should be(2)
  }

  "map" should "transform a Leaf to another Leaf" in {
    map(Leaf(1))(1.+) should be(Leaf(2))
  }

  it should "transform a Branch to another Branch of same structure" in {
    map(Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(3), Leaf(4))))(1.+) should be(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5))))
  }
}
