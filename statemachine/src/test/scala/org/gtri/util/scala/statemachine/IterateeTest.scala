package org.gtri.util.scala.statemachine

import org.scalatest.FunSpec
import org.gtri.util.scala.statemachine._
import scala.util.Random
import test._
import org.gtri.util.scala.statemachine.Enumerator.STD_CHUNK_SIZE
import scala.collection.immutable.Seq

class IterateeTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

  describe("A Iteratee[I,A]") {
    it("should consume input to produce a result") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val sum = l.foldLeft(0) { _ + _ }
      val i : Iteratee[Int,Int] = TestSumIntIteratee()
      // Note: utility functions called directly for testing purposes. An enumerator should be composed with the iteratee instead
      val result = utility.forceDoneTransition(utility.applySeqToState(i.s0, l))
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
      val result = utility.forceDoneTransition(utility.applySeqToState(i5.s0, l))
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
      val result = utility.forceDoneTransition(utility.applySeqToState(i5.s0, l))
      val isRecover =
        result.state match {
          case q : Iteratee.State.Continuation[Int,Int] => false
          case q : Iteratee.State.Halted[Int,Int] => q.optRecover.isDefined
          case q: Iteratee.State.Success[Int,Int] => false
        }
      assert(isRecover == true && result.overflow == l.drop(5))
      val result2 = utility.recoverTransition(utility.applySeqToState(result.state, result.overflow), shouldRecover = { (q: Iteratee.State.Halted[Int,Int]) => true }, maxAttempts = 4)
      val optSum : Option[Int] = result2.toOption
//      println(s"result2=$result2")
//      println(s"optSum=$optSum")
      assert(optSum.isDefined && optSum.get == sum)
    }
    it("should accumulate metadata") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val sum = l.foldLeft(0) { _ + _ }
      val i : Iteratee[Int,Int] = TestSumIntIteratee()
      // Note: utility functions called directly for testing purposes. An enumerator should be composed with the iteratee instead
      val result = utility.forceDoneTransition(utility.applySeqToState(i.s0, l))
      assert(result.metadata.length == (n * 2) + 2)
    }
    it("should be able to peek at a value using Iteratee.peek") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val sum = l(0) + l.take(10).foldLeft(0) { _ + _ }
      val i1 : Iteratee[Int,Int] = TestSumIntIteratee(10)
      val i2 : Iteratee[Int,Int] = for {
        v <- Iteratee.peek[Int]
        sum1 <- i1
      } yield v + sum1
      // Note: utility functions called directly for testing purposes. An enumerator should be composed with the iteratee instead
      val result = utility.forceDoneTransition(utility.applySeqToState(i2.s0, l))
      val optSum : Option[Int] = result.toOption
      assert(optSum.isDefined && optSum.get == sum)
    }

// TODO:
//    it("should be implicitly constructable from a function that takes input and returns a done State") {
//      import Iteratee._
//      def f(x : Int) : State.Done[Int,Int] = {
//        if(x != 0) {
//          State.Success(10 / x)
//        } else {
//          State.Halted(
//            issues = Issue.fatal("Divide by zero") :: Nil,
//            optRecover = Some(() =>
//              Succeed(10 / 1)
//            )
//          )
//        }
//      }
//      val n = STD_CHUNK_SIZE * 2
//      val l = rnd.take(n).toList
//      val sum = l(0) + l.take(10).foldLeft(0) { _ + _ }
//      val i1 : Iteratee[Int,Int] = TestSumIntIteratee(10)
//      val i2 : Iteratee[Int,Int] = f _
//      val i3 : Iteratee[Int,Int] = for {
//        sum1 <- i1
//        sum2 <- i2
//      } yield sum1 + sum2
//      // Note: utility functions called directly for testing purposes. An enumerator should be composed with the iteratee instead
//      val result = utility.forceDoneTransition(utility.applySeqToState(i3.s0, l, IssueRecoverStrategy.STRICT))
//      val optSum : Option[Int] = result.toOption
//      assert(optSum.isDefined && optSum.get == sum)
//
//    }
  }
}
