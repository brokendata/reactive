package design


// Contract for Monad
trait Monad[M[_]] extends Functor[M] {
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]
  def unit[A](a: A): M[A]
  def mapz[A,B](ma: M[A])(f: A =>B): M[B] =
    flatMap(ma)(x => unit(f(x)))
}

// Implicit instances of Monad in compation Object
object Monad {
  def apply[M[_]: Monad] = implicitly[Monad[M]]
  implicit val listMonad = new Monad[List] {
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
    def unit[A](a: A): List[A] = List(a)
  }
}