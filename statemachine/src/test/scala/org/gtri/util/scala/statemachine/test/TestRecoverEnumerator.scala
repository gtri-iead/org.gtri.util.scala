/*
    Copyright 2012 Georgia Tech Research Institute

    Author: lance.gatlin@gtri.gatech.edu

    This file is part of org.gtri.util.scala.statemachine library.

    org.gtri.util.scala.statemachine library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    org.gtri.util.scala.statemachine library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with org.gtri.util.scala.statemachine library. If not, see <http://www.gnu.org/licenses/>.

*/
package org.gtri.util.scala.statemachine.test

import org.gtri.util.scala.statemachine._

// Enumerator that will fail (with recover) every 5th item
object TestRecoverEnumerator {
  val log = Log(classOf[TestRecoverEnumerator[_]])
}
case class TestRecoverEnumerator[A](t: Traversable[A]) extends Enumerator[A] {
  import Enumerator._
  import TestRecoverEnumerator._

  case class Cont(n : Int, current : Traversable[A]) extends State.Continuation[A] {

    def apply(x : Unit) : Transition[A] = {
      val (nextChunk, remaining) = current.splitAt(1)
      if(remaining.isEmpty) {
        Succeed(
          output = nextChunk.toSeq,
          metadata = Seq(log.info("info1"),log.info("info2"))
        )
      } else {
        val r = Continue(
          state = Cont(n + 1,remaining),
          output = nextChunk.toSeq,
          metadata = Seq(log.info("info1"),log.info("info2"))
        )
        if(n % 5 == 0) {
          Halt.error(
            message = "error",
            cause = None,
            recover = () => r,
            metadata = Seq(log.error("error1"),log.info("error2"))
          )
        } else {
          r
        }

      }
    }
    def apply(x : EndOfInput) = Succeed(metadata = Seq(log.info("info1"),log.info("info2")))
  }

  def s0 = Cont(1, t)
}