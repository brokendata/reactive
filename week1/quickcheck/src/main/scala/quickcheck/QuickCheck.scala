package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("hint1") = forAll { (n: Int, m: Int) =>
    val h = insert(m, insert(n, empty))
    if (m < n) findMin(h) == m
    else findMin(h) == n
  }

  property("hint2") = forAll { (n: Int) =>
    val h = insert(n, empty)
    deleteMin(h) == empty
  }

  property("hint3") = forAll { (ls: List[Int]) =>
    def list2heap(ls: List[Int]): H = ls match {
      case Nil => empty
      case x :: xs => insert(x, list2heap(xs))
    }

    def heap2list(h: H): List[Int] = {
      if (isEmpty(h)) Nil
      else findMin(h) :: heap2list(deleteMin(h))
    }

    val sortedList = ls.sorted
    sortedList == heap2list(list2heap(ls))
  }

  property("hint4") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == findMin(h1) ||
      findMin(meld(h1, h2)) == findMin(h2)

  }
}
