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
import collection.immutable.NumericRange

class TranslatorTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

  describe("A Translator[I,O]") {
    it("should be composable with an Enumerator to form a new Enumerator") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val ls : List[String] = for(i <- l) yield i.toString
      val e : Enumerator[Int] = l.toEnumerator
      val t : Translator[Int,String] = TestIntToStringTranslator()
      val ei : Enumerator[String] = e compose t
      val result = ei.run()
      assert(result.output == ls)
    }
    it("should be composable with an Iteratee to form a new Iteratee") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val ls = l.foldLeft("") { _ + _.toString }
      val i : Iteratee[String,String] = TestAppendStringIteratee()
      val t : Translator[Int,String] = TestIntToStringTranslator()
      val it : Iteratee[Int,String] = t compose i
      // Note: utility functions called directly for testing purposes. An enumerator should be composed with the iteratee instead
      val result = utility.forceDoneTransition(utility.applyInputToState(it.s0, l, IssueRecoverStrategy.STRICT))
      val optSum : Option[String] = result.toOption
      assert(optSum.isDefined && optSum.get == ls)
    }
    it("should accumulate metadata") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val ls : List[String] = for(i <- l) yield i.toString
      val e : Enumerator[Int] = TestRecoverEnumerator(l) // (n * 2) + ((n / 5) * 2)
      val t : Translator[Int,String] = TestIntToStringTranslator() // (n * 2) + 2
      val et : Enumerator[String] = e compose t
      val result = et.run(shouldRecover = { _ => true })
      val eMetadataCount = (n * 2) + ((n/5)*2)
      val tMetadataCount = (n * 2) + 2
      assert(result.metadata.length == (eMetadataCount + tMetadataCount))
    }
    it("should accumulate Transitions in the correct order") {
      val abc = 'a' to 'z' map { _.toString }
      val cba = 'z'.toByte to 'a'.toByte by -1 map { _.toChar.toString }
      val tr : Translator[String,String] = TestAlphaTranslator()
      // Note: utility functions called directly for testing purposes. An enumerator should be composed with the iteratee instead
      val result : Translator.Transition[String,String] = utility.applyInputToState(tr.s0, abc.toList, IssueRecoverStrategy.STRICT)
      assert(result.output == cba)
    }

  }
}
