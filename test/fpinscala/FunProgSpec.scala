package fpinscala

import fpinscala.Main._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

/**
 * (c) Lectra.
 * @author c.delmas
 */
class FunProgSpec extends FlatSpec with Matchers {

  /** **************************/

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new mutable.Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be(2)
    stack.pop() should be(1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new mutable.Stack[Int]
    a[NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }

  /** **************************/

  "Factorial 5" should "return 120" in {
    factorial(5) should be(120)
  }

  "Fibo 7" should "return 21" in {
    fibo(7) should be(21)
  }

  "findFirst" should "return -1 for an empty array" in {
    findFirst(Array(), (x: Int) => x > 7)
  }

  it should "return 0 if the predicate matches the first element" in {
    findFirst(Array(0), (x: Int) => x < 7)
  }

  /** ***************************/

  "isSorted" should "find empty array sorted" in {
    isSorted(Array(), (s1: String, s2: String) => s1 > s2) should be(true)
  }

  it should "find 1,2,3 ordered with <" in {
    isSorted(Array(1,2,3), (i1:Int, i2:Int) => i1 < i2) should be(true)
  }

  it should "find 1,2,2,4,3,5 not ordered with <" in {
    isSorted(Array(1,2,2,4,3,5), (i1:Int, i2:Int) => i1 < i2) should be(false)
  }

  /********************************/

}
