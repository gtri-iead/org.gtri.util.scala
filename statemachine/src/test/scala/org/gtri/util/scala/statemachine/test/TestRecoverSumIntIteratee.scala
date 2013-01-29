package org.gtri.util.scala.statemachine.test

import org.gtri.util.scala.statemachine._

object TestRecoverSumIntIteratee {
  val log = Log(classOf[TestRecoverSumIntIteratee])
}
case class TestRecoverSumIntIteratee(maxN : Int = Int.MaxValue) extends Iteratee[Int, Int] {
  import Iteratee._
  import TestRecoverSumIntIteratee._
  case class Cont(n : Int, loopState : Int) extends State.Continue[Int, Int] {
    def apply(item : Int) = {
      if(n < maxN) {
        val r = Continue(
          state = new Cont(n + 1,loopState + item),
          metadata = Seq(log.info("info1"),log.info("info2"))
        )
        if(n % 5 == 0) {
          Failure(
            optRecover = Some(()=> r),
            metadata = Seq(log.info("info1"),log.info("info2"))
          )
        } else {
          r
        }
      } else {
        Success(
          value = loopState + item,
          metadata = Seq(log.info("info1"),log.info("info2"))
        )
      }
    }

    def apply(eoi : EndOfInput) = Success(
      value = loopState,
      metadata = Seq(log.info("info1"),log.info("info2"))
    )
  }
  def s0 = Cont(1, 0)
}

