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
import scala.collection.immutable.Seq

object TestRecoverSumIntIteratee {
  val log = Log(classOf[TestRecoverSumIntIteratee])
}
case class TestRecoverSumIntIteratee(maxN : Int = Int.MaxValue) extends Iteratee[Int, Int] {
  import Iteratee._
  import TestRecoverSumIntIteratee._
  case class Cont(n : Int, loopState : Int) extends State.Continuation[Int, Int] {
    def apply(item : Int) = {
//      println("n="+n+" item="+item+" loopState="+loopState)
      if(n < maxN) {
        val r = Continue(
          state = new Cont(n + 1,loopState + item),
          metadata = Seq(log.info("info1"),log.info("info2"))
        )
        if(n % 5 == 0) {
          Halt.error(
            message = "error",
            recover = ()=> r,
            metadata = Seq(log.info("info1"),log.info("info2"))
          )
        } else {
          r
        }
      } else {
        Succeed(
          value = loopState + item,
          metadata = Seq(log.info("info1"),log.info("info2"))
        )
      }
    }

    def apply(eoi : EndOfInput) = {
      println("EOI")
      Succeed(
        value = loopState,
        metadata = Seq(log.info("info1"),log.info("info2"))
      )
    }
  }
  def s0 = Cont(1, 0)
}

