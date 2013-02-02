package org.gtri.util.scala.statemachine

import org.scalatest.FunSpec
import org.gtri.util.scala.statemachine._
import scala.util.Random
import test._

class IterateeTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

  describe("A Iteratee[I,A]") {
    it("should consume input to produce a result") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val sum = l.foldLeft(0) { _ + _ }
      val i : Iteratee[Int,Int] = TestSumIntIteratee()
      // Note: utility functions called directly for testing purposes. An enumerator should be composed with the iteratee instead
      val result = utility.forceDoneTransition(utility.applyInputToState(i.s0, l, IssueRecoverStrategy.STRICT))
      val optSum : Option[Int] = result.toOption
      assert(optSum.isDefined && optSum.get == sum)
    }
    it("should be able to flatMap/map feeding overflow from Iteratee to the next Iteratee") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val sum = l.take(40).foldLeft(0) { _ + _ }
      val i1 : Iteratee[Int,Int] = TestSumIntIteratee(10)
      val i2 : Iteratee[Int,Int] = TestSumIntIteratee(10)
      val i3 : Iteratee[Int,Int] = TestSumIntIteratee(10)
      val i4 : Iteratee[Int,Int] = TestSumIntIteratee(10)
      val i5 : Iteratee[Int,Int] = for(sum1 <- i1;sum2 <- i2;sum3 <- i3;sum4 <- i4) yield sum1 + sum2 + sum3 + sum4
      // Note: utility functions called directly for testing purposes. An enumerator should be composed with the iteratee instead
      val result = utility.forceDoneTransition(utility.applyInputToState(i5.s0, l, IssueRecoverStrategy.STRICT))
      val optSum : Option[Int] = result.toOption
      assert(optSum.isDefined && optSum.get == sum)
    }
    it("should be able to recover from a failure during flatMap/map") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val sum = l.take(40).foldLeft(0) { _ + _ }
      val i1 : Iteratee[Int,Int] = TestRecoverSumIntIteratee(10)
      val i2 : Iteratee[Int,Int] = TestRecoverSumIntIteratee(10)
      val i3 : Iteratee[Int,Int] = TestRecoverSumIntIteratee(10)
      val i4 : Iteratee[Int,Int] = TestRecoverSumIntIteratee(10)
      val i5 : Iteratee[Int,Int] = for(sum1 <- i1;sum2 <- i2;sum3 <- i3;sum4 <- i4) yield sum1 + sum2 + sum3 + sum4
      // Note: utility functions called directly for testing purposes. An enumerator should be composed with the iteratee instead
      val result = utility.forceDoneTransition(utility.applyInputToState(i5.s0, l, IssueRecoverStrategy.STRICT))
      val isRecover =
        result.state match {
          case q : Iteratee.State.Continuation[Int,Int] => false
          case q : Iteratee.State.Halted[Int,Int] => q.optRecover.isDefined
          case q: Iteratee.State.Success[Int,Int] => false
        }
      assert(isRecover == true && result.overflow == l.drop(5))
      val result2 = utility.forceDoneTransition(utility.applyInputToState(result.state, result.overflow, shouldRecover = { (q: Iteratee.State.Halted[Int,Int]) => true }))
      val optSum : Option[Int] = result2.toOption(shouldRecover = IssueRecoverStrategy.LAX)
      assert(optSum.isDefined && optSum.get == sum)
    }
    it("should accumulate metadata") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val sum = l.foldLeft(0) { _ + _ }
      val i : Iteratee[Int,Int] = TestSumIntIteratee()
      // Note: utility functions called directly for testing purposes. An enumerator should be composed with the iteratee instead
      val result = utility.forceDoneTransition(utility.applyInputToState(i.s0, l, IssueRecoverStrategy.STRICT))
      assert(result.metadata.length == (n * 2) + 2)
    }
  }
}
