import java.util.Random

sealed trait Generator[+A]{self =>
  def generate: A

  def map[B](f: A =>B): Generator[B] = new Generator[B]{
    def generate = f(self.generate)
  }
  def flatMap[B](f: A => Generator[B]): Generator[B] = new Generator[B] {
    override def generate: B = f(self.generate).generate
  }

}


val inGen = new Generator[Int] {
  def generate: Int = {
    val rand = new java.util.Random
    rand.nextInt()
  }
}


