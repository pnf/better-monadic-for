package applicativish

import annotation.unchecked._
import scala.language.higherKinds
import scala.languageFeature.implicitConversions


trait TupleLifter[+M[_]] {
  def tupleLift[A,B](t: (M[A],M[B]) @uncheckedVariance): M[(A,B)] @uncheckedVariance
}
