package com.olegpy.bm4

import applicativish.TupleLifter
import applicativish.TupleLifters._

import scala.concurrent.Future
import scala.concurrent.duration.Duration
import cats.effect.IO
import monix.execution.Scheduler.Implicits.global
import monix.eval.Task
import cats.implicits._
import org.scalatest.{FreeSpec, FunSuite}


object StupidImplicits {

  implicit def toFunnyTupled[A, M[_] <: Option[_]](t: (M[A], M[A])) = new {
    def tupled: Option[(Int,Int)] = t match {
      case (Some(a: Int), Some(b: Int)) ⇒ Some(a*10, b*10)
      case _ ⇒ None
    }
  }

}


class TestFor extends FreeSpec {

  import com.olegpy.bm4.StupidImplicits._

  val xxx = implicitly[TupleLifter[Option]]
  val yyy= implicitly[TupleLifter[BogusOption]]

  val ljsfljfds = 2

/*
  class IntOptionWithTupled[A, M[_] <: Option[_]](t: (M[A], M[A])) {
    def tupled: Option[(Int,Int)] = t match {
      case (Some(a: Int), Some(b: Int)) ⇒ Some(a*10, b*10)
      case _ ⇒ None
    }
  }

  */
  (Some(2),Some(3)).tupled

  (null.asInstanceOf[Option[Int]], null.asInstanceOf[Option[Int]]).tupled


  val scuz = for {
    a ← Option(1)
    b ← Option(2)
    c ← Option(3)
  } yield a + b + c
  println(scuz)


  val x = for {
    a ← BogusOption(1)
    b ← BogusOption(2)
    c ← BogusOption(3)
  } yield a+b+c

  println(x)


  val y = scala.Some.apply[Int](1)
    .flatMap[Int](( (a: Int) =>
                    scala.Some.apply[Int](2).flatMap[Int]((
                       (b: Int) => scala.Some.apply[Int](a.+(b)).map[Int](((c: Int) => c))))))

  println(y)

  "Plugin allows" - {
    "destructuring for monads without withFilter" in {
      val task = for {
        (a, b) <- Task.zip2(Task("Hello"), Task("there"))
      } yield s"$a $b"

      assert(task.runSyncUnsafe(Duration.Inf) == "Hello there")
    }

    "also with lots of generators inside" - {
      val io = for {
        _ <- IO.unit
        (a, b) <- IO((1, 2))
        _ <- IO.unit
        s <- IO("Output")
        Some(sep) <- IO(Some(": "))
      } yield s"$s$sep${a + b}"
      assert(io.unsafeRunSync() == "Output: 3")
    }

    "and with nested for-s" in {
      val task = for {
        (a, b) <- Task.zip2(Task("Hello"), Task("there"))
        txt = s"$a $b!"
        io = for ((c, d) <- IO(("General", "Kenobi"))) yield s"$c $d!"
        txt2 <- io.to[Task]
      } yield s"$txt $txt2"

      assert(task.runSyncUnsafe(Duration.Inf) == "Hello there! General Kenobi!")
    }

    "easy type patterns on left hand side" - {
      "for one-liners" in (for (x: Int <- IO(42)) yield x)
      "for multiple lines" in {
        for {
          x: Int <- IO(42)
          s: String <- IO("Foo")
        } yield s + x
      }
    }

    "traversing through deeply nested definitions" in {
      object A {
        class B {
          val c: Unit = {
            def localMethod = for (x: Int <- IO(42)) yield x
            ()
          }
        }
      }
    }
  }

  // TODO: utest compileError does not use plugin
  "exhaustiveness checks" - {
    "option" in {
      for (Some(x) <- IO(none[Int])) yield x
    }
    "lists & singletons" in {
      object Singleton
      for (Singleton <- IO(Singleton); (a, Nil) <- IO((1, List(1)))) yield a
    }
  }

  "preserving" - {
    "if guards for things supporting it" in {
      val res = for {
        bool <- List(true, false, false, true)
        if bool
      } yield bool

      assert(res == List(true, true))
    }
  }
}
