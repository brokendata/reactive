package design




trait Functor[F[_]]{
  def mapz[A,B](fa: F[A])(f: A => B): F[B]
}
