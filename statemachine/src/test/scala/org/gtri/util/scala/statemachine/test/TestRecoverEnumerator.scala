package org.gtri.util.scala.statemachine.test

import org.gtri.util.scala.statemachine._

// Enumerator that will fail (with recover) every 5th item
object TestRecoverEnumerator {
  val log = Log(classOf[TestRecoverEnumerator[_]])
}
case class TestRecoverEnumerator[A](t: Traversable[A]) extends Enumerator[A] {
  import org.gtri.util.scala.statemachine.Enumerator._
  import TestRecoverEnumerator._

  case class ∂∂(n : Int, current : Traversable[A]) extends ∂[A] {

    def apply(x : Unit) : Result[A] = {
      val (nextChunk, remaining) = current.splitAt(1)
      if(remaining.isEmpty) {
        ⊡(
          output = nextChunk.toSeq,
          metadata = Seq(log.info("info1"),log.info("info2"))
        )
      } else {
        val r = ⊳(
          state = ∂∂(n + 1,remaining),
          output = nextChunk.toSeq,
          metadata = Seq(log.info("info1"),log.info("info2"))
        )
        if(n % 5 == 0) {
          ⊠(
            optRecover = Some(() => r),
            metadata = Seq(log.error("error1"),log.info("error2"))
          )
        } else {
          r
        }

      }
    }
    def apply(x : EndOfInput) = ⊡(metadata = Seq(log.info("info1"),log.info("info2")))
  }

  def s0 = ∂∂(1, t)
}