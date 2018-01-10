package nubank

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

object Utils {
  implicit class WithMapFold[A, T[B] <: TraversableOnce[B]](to: T[A]) {
    def mapFold[Z, C, That](z: Z)(f: (Z, A) => (C, Z))(implicit bf: CanBuildFrom[T[A], C, That]): (That, Z) = {
      val builder = bf(to)
      val r = to.foldLeft(z) { (z, a) =>
        val (c, z1) = f(z, a)
        builder += c
        z1
      }
      (builder.result(), r)
    }
  }
}
