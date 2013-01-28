package org.gtri.util.scala.statemachine

import org.scalatest.FunSpec
import org.gtri.util.scala.statemachine._
import scala.util.Random
import test.TestSumIntIteratee

class IterateeTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

  describe("A Iteratee[I,A]") {
    it("should consume input to produce a result") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val sum = l.foldLeft(0) { _ + _ }
//      println("sum="+sum)
      val e : Enumerator[Int] = l.toEnumerator
      val i : Iteratee[Int,Int] = TestSumIntIteratee()
      val ei : Iteratee[Unit,Int] = e compose i
      val result : Iteratee.Result[Unit,Int] = ei.run()
      val optSum : Option[Int] = result.toOption
//      println("result="+result)
//      println("optSum="+optSum)
      assert(optSum.isDefined && optSum.get == sum)
    }
    it("should be able to flatMap/map feeding overflow from Iteratee to the next inner iteratee") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val sum = l.take(20).foldLeft(0) { _ + _ }
//      println("sum="+sum)
      val e : Enumerator[Int] = l.toEnumerator
      val i1 : Iteratee[Int,Int] = TestSumIntIteratee(10)
      val i2 : Iteratee[Int,Int] = TestSumIntIteratee(10)
//      val i3 : Iteratee.State[Int,Int] = for(sum1 <- i1.s0;sum2 <- i2.s0) yield sum1 + sum2
      val i3 : Iteratee[Int,Int] = for(sum1 <- i1;sum2 <- i2) yield sum1 + sum2

//      val ei : Iteratee.State[Unit,Int] = e.s0 compose i3
      val ei : Iteratee[Unit,Int] = e compose i3
      val result = ei.run()
      val optSum : Option[Int] = result.toOption
//      println("result="+result)
//      println("optSum="+optSum)
      assert(optSum.isDefined && optSum.get == sum)

    }
  }
}
