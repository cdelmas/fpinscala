package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest.{FlatSpec, Matchers}

/**
 * (c) Lectra.
 * @author c.delmas
 */
class ListSpec extends FlatSpec with Matchers {

  "List" should " match properly" in {

    val res = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    res should be(3)
  }

  "tail" should "return Nil with Nil list" in {
    tail(Nil) should be(Nil)
  }

  it should "return the two remaining elements of a List of 3" in {
    tail(List("a", "b", "c")) should be(List("b", "c"))
  }

  "setHead" should "build a new Cons with parameter as head" in {
    setHead(0, List(1, 2, 3)) should be(List(0, 1, 2, 3))
  }

  "drop" should "return Nil on Nil" in {
    drop(Nil, 6) should be(Nil)
  }

  it should "return Nil if n >= list.length" in {
    drop(List(2, 3), 2) should be(Nil)
  }

  it should "return the sublist from index n if n <= list.length" in {
    drop(List(2, 3), 1) should be(List(3))
  }

  "dropWhile" should "return Nil on an empty List" in {
    dropWhile(Nil)((x: Int) => x > 3) should be(Nil)
  }

  it should "return the list if first element doesn't match" in {
    dropWhile(List(1, 2, 3))(x => x > 3) should be(List(1, 2, 3))
  }

  it should "remove the 2 first elements, the third is not matching" in {
    dropWhile(List(1, 2, 3))(x => x < 3) should be(List(3))
  }

  "init" should "be Nil on empty List" in {
    init(Nil) should be(Nil)
  }

  it should "be Nil for a list of 1 element" in {
    init(List("hello")) should be(Nil)
  }

  it should "get the List of length - 1 first elements if length > 2" in {
    init(List("hello", "world", "!")) should be(List("hello", "world"))
  }

  "foldRight" should "copy the list" in {
    val someInts = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    someInts should be(Cons(1, Cons(2, Cons(3, Nil))))
  }

  "length" should "be zero for an empty list" in {
    List.length(Nil) should be(0)
  }

  it should "return 5 for List(1,2,3,4,6)" in {
    List.length(List(1, 2, 3, 4, 6)) should be(5)
  }

  "sumLeft" should "give 0 for an empty list" in {
    sumLeft(Nil) should be(0)
  }

  it should "give 3 for List(1,1,1)" in {
    sumLeft(List(1, 1, 1)) should be(3)
  }

  "productLeft" should "give 6 for List(1,2,3)" in {
    productLeft(List(1, 2, 3)) should be(6)
  }

  "lengthLeft" should "be zero for an empty list" in {
    lengthLeft(Nil) should be(0)
  }

  it should "return 5 for List(1,2,3,4,6)" in {
    lengthLeft(List(1, 2, 3, 4, 6)) should be(5)
  }

  "reverse" should "give Nil with empty list" in {
    reverse(Nil) should be(Nil)
  }

  it should "give List(5,4,3,2,1) with List(1,2,3,4,5)" in {
    reverse(List(1, 2, 3, 4, 5)) should be(List(5, 4, 3, 2, 1))
  }

  "append" should "give a Cons(e) on empty list" in {
    append(Nil, List(5)) should be(List(5))
  }

  it should "add the given element at the end" in {
    append(List(0, 1, 2, 3), List(9)) should be(List(0, 1, 2, 3, 9))
  }

  "concatenate" should "make a simple list from two lists" in {
    concat(List(List("a"), List("b", "c", "d"))) should be(List("a", "b", "c", "d"))
  }

  "add1" should "give Nil with an empty list" in {
    add1(Nil) should be(Nil)
  }

  it should "add one to all elements of the list" in {
    add1(List(1, 2, 3, 3)) should be(List(2, 3, 4, 4))
  }

  "asString" should "give a toString list of elements" in {
    asString(List(4.0, 3.1, 5.5, 9.2)) should be(List("4.0", "3.1", "5.5", "9.2"))
  }

  "map" should "add 1 to each element given the add1 function" in {
    map(List(1, 2, 3))(x => x + 1) should be(List(2, 3, 4))
  }

  "filter" should "remove elements matching the predicate" in {
    filter(List(1, 2, 3, 4, 5))(x => x % 2 == 1)
  }

  "flatmap" should "double each int in a list" in {
    flatMap(List(1, 2, 3))(i => List(i, i)) should be(List(1, 1, 2, 2, 3, 3))
  }

  "combine" should "add elements from one list to elements of the other, index by index" in {
    combine(List(1, 2, 3), List(4, 5, 6)) should be(List(5, 7, 9))
  }

  "zipWith" should "do the same as combine with numbers and +" in {
    zipWith(List(4, 5, 6), List(1, 2, 3))(_ + _) should be(List(5, 7, 9))
  }

  "hasSubsequence" should "find the complete list to be a subsequence" in {
    hasSubsequence(List(1, 2, 3))(List(1, 2, 3)) should be
  }

  it should "find an element of the list to be a subsequence" in {
    hasSubsequence(List(1, 2, 3))(List(1)) should be
    hasSubsequence(List(1, 2, 3))(List(2)) should be
    hasSubsequence(List(1, 2, 3))(List(3)) should be
  }

  it should "find n successive elements to be a subsequence" in {
    hasSubsequence(List(1, 2, 3))(List(1, 2)) should be
    hasSubsequence(List(1, 2, 3))(List(1, 2)) should be
    hasSubsequence(List(1, 2, 3))(List(1, 2)) should be
  }

  it should "not find Nil as a subsequence" in {
    hasSubsequence(List(1, 2, 3))(Nil) should be(false)
  }

  it should "not find reversed list as a subsequence" in {
    hasSubsequence(List(1, 2, 3))(List(2, 1)) should be(false)
  }
}
