import java.util.Random

sealed trait Generator[+A]{self =>
  def generate: A
  def map[B](f: A => B): Generator[B] = new Generator[B] {
    def generate: B = f(self.generate)
  }

  def flatMap[B](f: A => Generator[B]): Generator[B] = new Generator[B] {
    def generate: B = f(self.generate).generate
  }
}
val intGen = new Generator[Int]{
  def generate: Int = {
    val r = new Random()
    r.nextInt
  }
}
intGen map (_ > 1)

for {
  v <- intGen
} yield v > 0


