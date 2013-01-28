package org.gtri.util.scala.statemachine

import org.scalatest.FunSpec
import org.gtri.util.scala.statemachine._
import scala.util.Random
import test.{TestIntToStringTranslator, TestAppendStringIteratee, TestSumIntIteratee}

class IterateeTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

  describe("A Iteratee[I,A]") {
    it("should consume input to produce a result") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val sum = l.foldLeft(0) { _ + _ }
      val e : Enumerator[Int] = l.toEnumerator
      val i : Iteratee[Int,Int] = TestSumIntIteratee()
      val ei : Plan[Int] = e compose i
      val result : Plan.Result[Int] = ei.run()
      val optSum : Option[Int] = result.toOption
      assert(optSum.isDefined && optSum.get == sum)
    }
    it("should be able to flatMap/map feeding overflow from Iteratee to the next Iteratee") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val sum = l.take(40).foldLeft(0) { _ + _ }
      val e : Enumerator[Int] = l.toEnumerator
      val i1 : Iteratee[Int,Int] = TestSumIntIteratee(10)
      val i2 : Iteratee[Int,Int] = TestSumIntIteratee(10)
      val i3 : Iteratee[Int,Int] = TestSumIntIteratee(10)
      val i4 : Iteratee[Int,Int] = TestSumIntIteratee(10)
      val i5 : Iteratee[Int,Int] = for(sum1 <- i1;sum2 <- i2;sum3 <- i3;sum4 <- i4) yield sum1 + sum2 + sum3 + sum4
      val ei : Plan[Int] = e compose i5
      val result = ei.run()
      val optSum : Option[Int] = result.toOption
      assert(optSum.isDefined && optSum.get == sum)
    }
    it("should be able to be composed with a Translator to form a new Iteratee") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val ls = l.foldLeft("") { _ + _.toString }
      val e : Enumerator[Int] = l.toEnumerator
      val i : Iteratee[String,String] = TestAppendStringIteratee()
      val t : Translator[Int,String] = TestIntToStringTranslator()
      val it : Iteratee[Int,String] = t compose i
      val eit : Plan[String] = e compose it
      val result = eit.run()
      val optSum : Option[String] = result.toOption
      assert(optSum.isDefined && optSum.get == ls)
    }
  }
}
