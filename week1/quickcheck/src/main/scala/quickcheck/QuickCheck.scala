package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = oneOf(const(empty), genHeap)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)




  property("hint1") = forAll { (n: Int, m: Int) =>
    val h = insert(m,insert(n,empty))
    if (m < n) findMin(h) == m
    else findMin(h) == n
  }

  property("hint2") = forAll{ (n: Int) =>
    val h = insert(n,empty)
    deleteMin(h) == empty
  }



}
