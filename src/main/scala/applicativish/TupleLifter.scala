package applicativish

import annotation.unchecked._
import scala.language.higherKinds


trait TupleLifter[-M[_]] {
  def tupleLift[A,B](t: (M[A],M[B])): M[(A,B)] @uncheckedVariance
}

object TupleLifters {

  case class BogusOption[+A](a: A) {
    def map[B](f: A ⇒ B): BogusOption[B] = BogusOption(f(a))
    def flatMap[B](f: A ⇒ BogusOption[B]): BogusOption[B] = f(a)
  }

  implicit object BogusOptionLifter extends TupleLifter[BogusOption] {
      override def tupleLift[A, B](t: (BogusOption[A], BogusOption[B])): BogusOption[(A, B)] = t match {
        case (BogusOption(a: Int), BogusOption(b: Int)) ⇒ BogusOption((10*a, 10*b)).asInstanceOf[BogusOption[(A,B)]]
        case _ ⇒ throw new Exception
      }
    }

  implicit object OptionTupleLifter extends TupleLifter[Option] {
    override def tupleLift[A, B](t: (Option[A], Option[B])): Option[(A, B)] =
      t._1.flatMap {a: A ⇒ t._2.map { b : B⇒ (a,b) } }
  }

}

