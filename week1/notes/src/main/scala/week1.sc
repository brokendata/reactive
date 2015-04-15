import scala.language.higherKinds
import design.Monad
// We can add a `mapz` method to to list via implicit Enhancement
object MonadSyntax{
  import design.Monad
  // enrich any implicit instance of design.Monad with a mapz and unit method
  implicit class MSyntax[M[_]: Monad,A](m: M[A]){
    def mapz[B](f: A=>B): M[B] = implicitly[Monad[M]].mapz(m)(f)
  }
  // enrich any type with a `point` method ala scalaz
  implicit class MUnitBuilder[A](a: A){
    def point[M[_]:Monad]: M[A] = implicitly[Monad[M]].unit(a)
  }
}

import MonadSyntax._
//Both calling the typeclassing directly and implicitly work
Monad[List].mapz(List(1,2,3))(_+1)
List(1,2,3,4) mapz (_+1)
// Monad Laws -------------------
// Associative Law
// x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

List(1).flatMap(x  => List(x +1)).flatMap(y => List(y + 1)) ==
List(1).flatMap(x => List(x +1).flatMap(y => List(y +1)))
// Right Identity
List(1).flatMap(_.point[List]) == List(1)

