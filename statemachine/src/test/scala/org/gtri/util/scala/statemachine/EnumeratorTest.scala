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
import org.gtri.util.scala.statemachine.Enumerator.STD_CHUNK_SIZE
import scala.collection.immutable.Seq

class EnumeratorTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

  describe("A Enumerator[O]") {
    it("should provide a single result chunk when stepped") {
      val n = STD_CHUNK_SIZE
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = l.toEnumerator
      val result = e.s0.step()
      assert(result.output == l)
    }
    it("should contain all output in result after run") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = l.toEnumerator
      val state0 = e.s0
      val result = state0.run()
      assert(result.output == l)
    }
    it("should be able to recover from failure") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = TestRecoverEnumerator(l)
      val result = e.run()
      val isRecover =
        result.state match {
          case q : Enumerator.State.Continuation[Int] => false
          case q : Enumerator.State.Halted[Int] => q.optRecover.isDefined
          case q: Enumerator.State.Success[Int] => false
        }
      assert(isRecover == true)
      val (result2,_) = result.state.run(HaltedRecoveryStrategy.LAX)
      assert(result.output ++ result2.output == l)
    }
    it("should accumulate metadata") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = TestRecoverEnumerator(l)
      // chunks#=2
      // first iter=recover +2 metadata
      // second iter=continuation +2 metadata
      val (result,_) = e.run(HaltedRecoveryStrategy.LAX)
      val metadataFromTestRecoverEnumerator = (n/5)*2
      val metadataFromIssues = (n/5)
      val metadataFromContinuation = n * 2
      val totalMetadata = metadataFromTestRecoverEnumerator + metadataFromIssues + metadataFromContinuation
      assert(result.metadata.length == totalMetadata)
    }
  }
}


