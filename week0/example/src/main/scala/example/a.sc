def max(xs: List[Int]): Int = {
  def loop(xs: List[Int], max: Int): Int = xs match {
    case Nil => max
    case x :: xs => loop(xs, if (x > max) x else max)
  }
  loop(xs.tail, xs.head)
}
max(List(4,1,2,3))