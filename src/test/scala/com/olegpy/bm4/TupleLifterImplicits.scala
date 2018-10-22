package com.olegpy.bm4

import applicativish.TupleLiftable

import scala.reflect.internal.Flags

object TupleLifterImplicits {



  implicit object OptionTupleLifter extends TupleLiftable[Option] {
    override def tupleLift[A, B](t: (Option[A], Option[B])): Option[(A, B)] = t match {
      case (Some(a: Int), Some(b: Int)) ⇒ Some((10 * a, 10 * b)).asInstanceOf[Option[(A, B)]]
      case _ ⇒ t._1.flatMap( a ⇒ t._2.map(b ⇒ (a,b)))
    }
  }


}
