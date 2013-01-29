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
package org.gtri.util.scala.statemachine

import org.scalatest.FunSpec
import org.gtri.util.scala.statemachine._
import scala.util.Random
import test._

class TraversableEnumeratorTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

  describe("A TraversableEnumerator[O]") {
    it("should be be constructable from any traversable") {
      val e : Enumerator[Int] = rnd.toEnumerator
    }
    it("should end with the Success state once input is exhausted") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = l.toEnumerator
      val state0 : Enumerator.State[Int] = e.s0
      val result = state0.run()
      val isSuccess =
        result.state match {
          case q : Enumerator.State.Continue[Int] => false
          case q : Enumerator.State.Success[Int] => true
          case q : Enumerator.State.Failure[Int] => false
        }
      // Just demo'ing a second way of doing this
      val isSuccess2 =
        result.state.fold(
          ifContinue = { q => false },
          ifSuccess = { q => true },
          ifFailure = { q => false }
        )
      assert(isSuccess == true && isSuccess2 == true)
    }
    it("should return Success state when endOfInput is applied to a continue state") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = l.toEnumerator
      val state0 = e.s0
      val result = utility.forceDoneState(state0)
      val isSuccess =
        result.state.fold(
          ifContinue = { q => false },
          ifSuccess = { q => true },
          ifFailure = { q => false }
        )
      assert(isSuccess == true)
    }
  }
}
