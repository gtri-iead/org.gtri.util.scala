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
import scala.util.Random
import test._
import org.gtri.util.scala.statemachine.Enumerator.STD_CHUNK_SIZE
import scala.collection.immutable.Seq

class PlanTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

  describe("A Plan[A]") {
    it("should be composable from an Enumerator and an Iteratee") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val sum = l.foldLeft(0) { _ + _ }
      val e : Enumerator[Int] = l.toEnumerator
      val i : Iteratee[Int,Int] = TestSumIntIteratee()
      val ei : Plan[Int] = e compose i
      val result = ei.run()
      val opt = result.state.toOption
      assert(opt.isDefined && opt.get == sum)
    }
    it("should be composable from an Enumerator, any number of Translators and an Iteratee") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val sum = l.foldLeft("") { _ + _.toString }
      val e : Enumerator[Int] = l.toEnumerator
      val t : Translator[Int,String] = TestIntToStringTranslator()
      val i : Iteratee[String,String] = TestAppendStringIteratee()
      val eit : Plan[String] = e compose t compose i
      val result = eit.run()
      val opt = result.state.toOption
      assert(opt.isDefined && opt.get == sum)
    }
    it("should be able to recover from failure") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val sum = l.foldLeft(0) { _ + _ }
      val e : Enumerator[Int] = TestRecoverEnumerator(l)
      val i : Iteratee[Int,Int] = TestRecoverSumIntIteratee()
      val ei : Plan[Int] = e compose i
      val result = ei.run()
      val isRecover =
        result.state match {
          case q : Plan.State.Continuation[Int] => false
          case q : Plan.State.Halted[Int] => q.optRecover.isDefined
          case q: Plan.State.Success[Int] => false
        }
      assert(isRecover == true)
      val (result2,_) = result.state.run(HaltedRecoveryStrategy.LAX)
      val opt = result2.state.toOption
      assert(opt.isDefined && opt.get == sum)
    }
    it("should accumulate metadata") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val sum = l.foldLeft("") { _ + _.toString }
      val e : Enumerator[Int] = TestRecoverEnumerator(l) // (n * 2) + ((n / 5) * 2)
      val t : Translator[Int,String] = TestIntToStringTranslator()  // (n * 2) + 2
      val i : Iteratee[String,String] = TestAppendStringIteratee() // (n * 2) + 2
      val eit : Plan[String] = e compose t compose i
      val (result,_) = eit.run(HaltedRecoveryStrategy.LAX)

      val metadataFromTestRecoverEnumerator = (n/5)*2
      val metadataFromIssues = (n/5)
      val metadataFromContinuation = n * 2
      val totalMetadata = metadataFromTestRecoverEnumerator + metadataFromIssues + metadataFromContinuation

      val eMetadataCount = {
        val metadataFromTestRecoverEnumerator = (n/5)*2
        val metadataFromIssues = (n/5)
        val metadataFromContinuation = n * 2
        metadataFromTestRecoverEnumerator + metadataFromIssues + metadataFromContinuation
      }
      val tMetadataCount = {
        val metadataFromContinuation = n * 2
        val metadataFromEOI = 2
        metadataFromContinuation + metadataFromEOI
      }
      val iMetadataCount = {
        val metadataFromContinuation = n * 2
        val metadataFromEOI = 2
        metadataFromContinuation + metadataFromEOI
      }
      assert(result.metadata.length == (eMetadataCount + tMetadataCount + iMetadataCount))
    }
  }
}
