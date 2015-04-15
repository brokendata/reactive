import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import quickcheck._


val i = new IntHeap with BinomialHeap


val x = i.empty
val x2 = i.insert(3,x)

i.findMin(x2)


val genHeap: Gen[H] = for {
  v <- arbitrary[Int]
  x <- oneOf(const(v.emptu))
}
