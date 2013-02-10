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
      val result = utility.forceDoneTransition(utility.applySeqToState(l,i.s0))
      val optSum : Option[Int] = result.state.toOption
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
      val result = utility.applySeqToState(l,i5.s0)
      val optSum : Option[Int] = result.state.toOption
      assert(optSum.isDefined && optSum.get == sum)
    }
    it("should be able to recover from a failure during flatMap/map [2/2]") {
      // This test makes use of Plan to test overflow of composed Halted states
      val n = STD_CHUNK_SIZE * 2
//      val l = rnd.take(n).toList
      val l = List.fill(40)(1)
      val sum = l.take(40).foldLeft(0) { _ + _ }
      val i1 : Iteratee[Int,Int] = TestRecoverSumIntIteratee(10)
      val i2 : Iteratee[Int,Int] = TestRecoverSumIntIteratee(10)
      val i3 : Iteratee[Int,Int] = TestRecoverSumIntIteratee(10)
      val i4 : Iteratee[Int,Int] = TestRecoverSumIntIteratee(10)
      val i5 : Iteratee[Int,Int] = for(sum1 <- i1;sum2 <- i2;sum3 <- i3;sum4 <- i4) yield sum1 + sum2 + sum3 + sum4
      val result = utility.applySeqToState(l,i5.s0)
      assert(result.state.isHalted && result.state.isRecoverable)
      val s : Iteratee.State.Halted[Int,Int] = result.state.asInstanceOf[Iteratee.State.Halted[Int,Int]]
      val f : Plan.State.Halted[Int] => Boolean = { q => true }
      val plan2 = result.overflow.toEnumerator compose s
      val (result2,_) = plan2.run(f)
      val optSum : Option[Int] = result2.state.toOption
      assert(optSum.isDefined && optSum.get == sum)
    }
    it("should be able to recover from a failure during flatMap/map [1/2]") {
      // This test uses utility directly to isolate testing of flatMap/map from issues that might occur using compose & Plan
      val n = STD_CHUNK_SIZE * 2
      val f : Iteratee.State.Halted[Int,Int] => Boolean = { q => true }
      val l = rnd.take(n).toList
      val sum = l.take(20).foldLeft(0) { _ + _ }
      val i1 : Iteratee[Int,Int] = TestRecoverSumIntIteratee(10)
      val i2 : Iteratee[Int,Int] = TestRecoverSumIntIteratee(10)
      val i3 : Iteratee[Int,Int] = for(sum1 <- i1;sum2 <- i2) yield { sum1 + sum2 }
      val result1 = utility.applySeqToState(l,i3.s0)
      assert(result1.state.isHalted && result1.state.isRecoverable)
      val result2 = utility.recoverAll(result1.state.asInstanceOf[Iteratee.State.Halted[Int,Int]], f, Int.MaxValue)
      assert(result2.state.isContinuation)
      val result3 = utility.applySeqToState(result1.overflow,result2.state)
      assert(result3.state.isHalted && result3.state.isRecoverable)
      val result4 = utility.recoverAll(result3.state.asInstanceOf[Iteratee.State.Halted[Int,Int]], f, Int.MaxValue)
      assert(result4.state.isContinuation)
      val result5 = utility.applySeqToState(result3.overflow,result4.state)
      assert(result5.state.isSuccess)
      val optSum : Option[Int] = result5.state.toOption
      assert(optSum.isDefined && optSum.get == sum)
    }

    it("should accumulate metadata") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val i : Iteratee[Int,Int] = TestSumIntIteratee()
      val result = utility.forceDoneTransition(utility.applySeqToState(l,i.s0))
      val iMetadataCount = {
        val metadataFromContinuation = n * 2
        val metadataFromEOI = 2
        metadataFromContinuation + metadataFromEOI
      }
      val totalMetadataCount = iMetadataCount
      println("result.metadata.length="+result.metadata.length)
      println("totalMetadataCount="+totalMetadataCount)
      assert(result.metadata.length == totalMetadataCount)
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
      val result = utility.applySeqToState(l,i2.s0)
      val optSum : Option[Int] = result.state.toOption
      assert(optSum.isDefined && optSum.get == sum)
    }

    it("should be implicitly constructable from a function that takes Input and returns a done State") {
      import Iteratee._
      def testSumF(n : Int, sum : Int) : Input[Int] => Transition[Int,Int] = {
        case Chunk(xs) =>
          val xss = xs.take(10 - n)
          val newSum = xss.foldLeft(sum) { _ + _ }
          if(n + xss.length >= 10) {
            Succeed(newSum)
          } else {
            Continue(testSumF(n + xss.length, newSum))
          }

        case EndOfInput => Succeed(sum)
      }
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val sum = l.take(20).foldLeft(0) { _ + _ }
      val i1 : Iteratee[Int,Int] = TestSumIntIteratee(10)
      val i2 : Iteratee[Int,Int] = testSumF(0,0)
      val i3 : Iteratee[Int,Int] = for {
        sum1 <- i1
        sum2 <- i2
      } yield sum1 + sum2
      val result = utility.applySeqToState(l,i3.s0)
      val optSum : Option[Int] = result.state.toOption
      assert(optSum.isDefined && optSum.get == sum)

    }
  }
}
