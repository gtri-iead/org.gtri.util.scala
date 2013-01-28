package org.gtri.util.scala.statemachine

import org.scalatest.FunSpec
import org.gtri.util.scala.statemachine._
import scala.util.Random
import test.{TestIntToStringTranslator, TestSumIntIteratee}

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
    it("should be able to be composed with an Iteratee to form a Plan") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val sum = l.foldLeft(0) { _ + _ }
      val e : Enumerator[Int] = l.toEnumerator
      val i : Iteratee[Int,Int] = TestSumIntIteratee()
      val ei : Plan[Int] = e compose i
      val result = ei.run()
      val opt = result.toOption
      assert(opt.isDefined && opt.get == sum)
    }
    it("should be able to be composed with a Translator to form a Enumerator") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val ls : List[String] = for(i <- l) yield i.toString
      val e : Enumerator[Int] = l.toEnumerator
      val t : Translator[Int,String] = TestIntToStringTranslator()
      val ei : Enumerator[String] = e compose t
      val result = ei.run()
      assert(result.output == ls)
    }
  }
}

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
