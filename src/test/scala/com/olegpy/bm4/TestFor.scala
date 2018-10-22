package com.olegpy.bm4

import applicativish.TupleLiftable

import scala.concurrent.Future
import scala.concurrent.duration.Duration
import cats.effect.IO
import monix.execution.Scheduler.Implicits.global
import monix.eval.Task
import cats.implicits._
import org.scalatest.{FreeSpec, FunSuite}
import scala.languageFeature.experimental.macros

import TupleLifterImplicits._

object TestApplicativish2 {
  import applicativish.TupleLiftable.liftTuples
  import TupleLifterImplicits.OptionTupleLifter

  def scuz = liftTuples { for {
    a ← Option(1)
    b ← Option(2)
    c ← Option(3)
  } yield a + b + c }



}

object TestApplicativish {

  val sbtnudge = 3

  // For some reason, the implicit is only discovered by the plugin if defined in the object scope rather
  // than imported.


  implicit val fooey = implicitly[TupleLiftable[Option]]

  def scuz = for {
    a ← Option(1)
    b ← Option(2)
    c ← Option(3)
  } yield a + b + c



}

class TestFor extends FreeSpec {

  assert(TestApplicativish.scuz == Some(51))
  assert(TestApplicativish2.scuz == Some(51))

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
